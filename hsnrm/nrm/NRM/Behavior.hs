{-|
Module      : NRM.Behavior
Copyright   : (c) UChicago Argonne, 2019
License     : BSD3
Maintainer  : fre@freux.fr
-}
module NRM.Behavior
  ( -- * NRM's core logic.
    behavior
  , -- * The Event specification
    NRMEvent (..)
  , -- * The Behavior specification
    Behavior (..)
  , CmdStatus (..)
  )
where

import qualified CPD.Core as CPD
import qualified CPD.Utils as CPD
import qualified CPD.Values as CPD
import Control.Lens hiding (to)
import Data.Generics.Product
import Data.MessagePack
import qualified NRM.CPD as NRMCPD
import qualified NRM.Classes.Messaging as M
import NRM.Sensors
import NRM.State
import NRM.Types.Cmd
import qualified NRM.Types.Configuration as Cfg
import qualified NRM.Types.DownstreamCmd as DC
import NRM.Types.LMap as LM
import qualified NRM.Types.Manifest as Manifest
import qualified NRM.Types.Messaging.DownstreamEvent as DEvent
import qualified NRM.Types.Messaging.UpstreamPub as UPub
import qualified NRM.Types.Messaging.UpstreamRep as URep
import qualified NRM.Types.Messaging.UpstreamReq as UReq
import NRM.Types.Process as Process
import qualified NRM.Types.Sensor as Sensor
import qualified NRM.Types.Slice as Ct
import NRM.Types.State
import qualified NRM.Types.Units as U
import qualified NRM.Types.UpstreamClient as UC
import Protolude

-- | The Behavior datatype describes an event from the runtime on which to react.
data NRMEvent
  = -- | A Request was received on the Upstream API.
    Req UC.UpstreamClientID UReq.Req
  | -- | Registering a child process.
    RegisterCmd CmdID CmdStatus
  | -- | Event from the application side.
    DownstreamEvent DC.DownstreamCmdID DEvent.Event
  | -- | Stdin/stdout data from the app side.
    DoOutput CmdID URep.OutputType Text
  | -- | Child death event
    ChildDied ProcessID ExitCode
  | -- | Sensor callback
    DoSensor
  | -- | Control loop calback
    DoControl
  | -- | Shutting down the daemon
    DoShutdown

-- | The Launch status of a command, for registration.
data CmdStatus
  = -- | In case the command to start a child succeeded, mark it as registered and provide its PID.
    Launched ProcessID
  | -- | In case the command to start a child failed.
    NotLaunched
  deriving (Generic, MessagePack)

-- | The Behavior datatype encodes a behavior to be executed by the NRM runtime.
data Behavior
  = -- | The No-Op
    NoBehavior
  | -- | Log a message
    Log Text
  | -- | Reply to an upstream client.
    Rep UC.UpstreamClientID URep.Rep
  | -- | Publish messages on upstream
    Pub [UPub.Pub]
  | -- | Start a child process
    StartChild CmdID Command Arguments Env
  | -- | Kill children processes and send some messages back upstream.
    KillChildren [CmdID] [(UC.UpstreamClientID, URep.Rep)]
  | -- | Pop one child process and may send a message back upstream.
    ClearChild CmdID (Maybe (UC.UpstreamClientID, URep.Rep))
  deriving (Show, Generic)

mayRep :: Cmd -> URep.Rep -> Behavior
mayRep c rep =
  (upstreamClientID . cmdCore) c & \case
    Just ucID -> Rep ucID rep
    Nothing -> Log "This command does not have a registered upstream client."

-- | The behavior function contains the main logic of the NRM daemon. It changes the state and
-- produces an associated behavior to be executed by the runtime. This contains the slice
-- management logic, the sensor callback logic, the control loop callback logic.
behavior :: Cfg.Cfg -> NRMState -> NRMEvent -> IO (NRMState, Behavior)
behavior _ st (DoOutput cmdID outputType content) =
  return . swap $ st &
    _cmdID cmdID
      \case
        Nothing -> (Log "No such command was found in the NRM state.", Nothing)
        Just c ->
          content & \case
            "" ->
              let newPstate =
                    outputType & \case
                      URep.StdoutOutput -> (processState c) {stdoutFinished = True}
                      URep.StderrOutput -> (processState c) {stderrFinished = True}
               in isDone newPstate & \case
                    Just exc -> (mayRep c (URep.RepCmdEnded $ URep.CmdEnded exc), Nothing)
                    Nothing -> (NoBehavior, Just (c & field @"processState" .~ newPstate))
            _ -> (respondContent content c cmdID outputType, Just c)
behavior _ st (RegisterCmd cmdID cmdstatus) =
  cmdstatus & \case
    NotLaunched ->
      registerFailed cmdID st & \case
        Just (st', _, _, cmdCore) ->
          upstreamClientID cmdCore & \case
            Just ucid -> bhv st' $ Rep ucid $ URep.RepStartFailure URep.StartFailure
            Nothing -> noBhv st
        Nothing -> noBhv st
    Launched pid ->
      mayLog st $
        registerLaunched cmdID pid st <&> \(st', sliceID, maybeClientID) ->
        fromMaybe (st', NoBehavior) $
          maybeClientID <&> \clientID ->
          ( st'
          , Rep clientID (URep.RepStart (URep.Start sliceID cmdID))
          )
behavior c st (Req clientid msg) =
  msg & \case
    UReq.ReqCPD _ -> justReply st clientid . URep.RepCPD . URep.CPD $ NRMCPD.toCPD st
    UReq.ReqSliceList _ -> bhv st . Rep clientid . URep.RepList . URep.SliceList . LM.toList $ slices st
    UReq.ReqGetState _ -> bhv st . Rep clientid . URep.RepGetState $ URep.GetState st
    UReq.ReqGetConfig _ -> bhv st . Rep clientid . URep.RepGetConfig $ URep.GetConfig c
    UReq.ReqRun UReq.Run {..} -> do
      cmdID <- nextCmdID <&> fromMaybe (panic "couldn't generate next cmd id")
      let (runCmd, runArgs) =
            (cmd spec, args spec) &
              ( if Manifest.perfwrapper (Manifest.app manifest) /=
                Manifest.PerfwrapperDisabled
              then wrapCmd (Cfg.argo_perf_wrapper c)
              else identity
              )
      return
        ( registerAwaiting cmdID
            ( mkCmd spec manifest
              ( if detachCmd
              then Nothing
              else Just clientid
              )
            )
            runSliceID .
            createSlice runSliceID $
            st
        , StartChild cmdID runCmd runArgs
          (env spec <> Env [("NRM_cmdID", toText cmdID)])
        )
    UReq.ReqKillSlice UReq.KillSlice {..} -> do
      let (maybeSlice, st') = removeSlice killSliceID st
      return
        ( st'
        , fromMaybe (Rep clientid $ URep.RepNoSuchSlice URep.NoSuchSlice)
          ( maybeSlice <&> \slice ->
            KillChildren (LM.keys $ Ct.cmds slice) $
              (clientid, URep.RepSliceKilled (URep.SliceKilled killSliceID)) :
              catMaybes
                ( (upstreamClientID . cmdCore <$> LM.elems (Ct.cmds slice)) <&>
                  fmap (,URep.RepThisCmdKilled URep.ThisCmdKilled)
                )
          )
        )
    UReq.ReqSetPower _ -> return (st, NoBehavior)
    UReq.ReqKillCmd UReq.KillCmd {..} ->
      removeCmd (KCmdID killCmdID) st & \case
        Nothing -> bhv st $ Rep clientid (URep.RepNoSuchCmd URep.NoSuchCmd)
        Just (info, _, cmd, sliceID, st') ->
          bhv st' $
            KillChildren [killCmdID] $
            ( clientid
            , info & \case
              CmdRemoved -> URep.RepCmdKilled (URep.CmdKilled killCmdID)
              SliceRemoved -> URep.RepSliceKilled (URep.SliceKilled sliceID)
            ) :
            maybe [] (\x -> [(x, URep.RepThisCmdKilled URep.ThisCmdKilled)])
              (upstreamClientID . cmdCore $ cmd)
behavior _ st (ChildDied pid exitcode) =
  lookupProcess pid st & \case
    Nothing -> bhv st $ Log "No such PID in NRM's state."
    Just (cmdID, cmd, sliceID, slice) ->
      let newPstate = (processState cmd) {ended = Just exitcode}
       in isDone newPstate & \case
            Just _ ->
              removeCmd (KProcessID pid) st & \case
                Just (_, _, _, _, st') ->
                  bhv st' $
                    ClearChild cmdID
                      ( (,URep.RepCmdEnded (URep.CmdEnded exitcode)) <$>
                        (upstreamClientID . cmdCore $ cmd)
                      )
                Nothing -> bhv st $ panic "Error during command removal from NRM state"
            Nothing ->
              noBhv $
                insertSlice sliceID
                  (Ct.insertCmd cmdID cmd {processState = newPstate} slice)
                  st
behavior _ st DoSensor = bhv st NoBehavior
behavior _ st DoControl = bhv st NoBehavior
behavior _ st DoShutdown = bhv st NoBehavior
behavior cfg st (DownstreamEvent clientid msg) =
  msg & \case
    DEvent.ThreadProgress _ _ -> return (st, Log "unimplemented")
    DEvent.ThreadPhaseContext _ _ -> return (st, Log "unimplemented")
    DEvent.ThreadPause _ -> return (st, Log "unimplemented")
    DEvent.ThreadPhasePause _ -> return (st, Log "unimplemented")
    DEvent.CmdPerformance DEvent.CmdHeader {..} DEvent.Performance {..} ->
      return $
        passiveSensorBehavior cfg st (DC.toSensorID clientid)
          (U.fromOps perf & fromIntegral) & \case
        Nothing ->
          registerDownstreamCmdClient cmdID clientid st <&>
            (,Log "downstream cmd client registered.") &
            fromMaybe (st, Log "no such Cmd")
        Just x ->
          x & _2 %~ \toPublish ->
            Pub $ toPublish :
              [ UPub.PubPerformance cmdID
                  DEvent.Performance
                    { DEvent.perf = perf
                    }
              ]
    DEvent.CmdPause DEvent.CmdHeader {..} ->
      unRegisterDownstreamCmdClient cmdID clientid st & \case
        Just st' -> bhv st' $ Log "downstream cmd client un-registered."
        Nothing -> bhv st $ Log "No corresponding command for this downstream cmd 'pause' request."

noBhv :: a -> IO (a, Behavior)
noBhv = flip bhv NoBehavior

bhv :: Monad m => a -> b -> m (a, b)
bhv st x = return (st, x)

justReply :: Monad m => a -> UC.UpstreamClientID -> URep.Rep -> m (a, Behavior)
justReply st clientID = bhv st . Rep clientID

-- | The sensitive unpacking that has to be pattern-matched on the python side.
-- These toObject/fromObject functions do not correspond to each other and the instance
-- just exists for passing the behavior to the python runtime.
instance MessagePack Behavior where

  toObject NoBehavior = toObject ("noop" :: Text)
  toObject (Log msg) = toObject ("log" :: Text, msg)
  toObject (Pub msgs) = toObject ("publish" :: Text, M.encodeT <$> msgs)
  toObject (Rep clientid msg) =
    toObject ("reply" :: Text, clientid, M.encodeT msg)
  toObject (StartChild cmdID cmd args env) =
    toObject ("cmd" :: Text, cmdID, cmd, args, env)
  toObject (KillChildren cmdIDs reps) =
    toObject
      ( "kill" :: Text
      , cmdIDs
      , (\(clientid, msg) -> (clientid, M.encodeT msg)) <$> reps
      )
  toObject (ClearChild cmdID maybeRep) =
    toObject
      ( "pop" :: Text
      , cmdID
      , (\(clientid, msg) -> (clientid, M.encodeT msg)) <$> Protolude.toList maybeRep
      )

  fromObject x = to <$> gFromObject x

mayLog :: NRMState -> Either Text (NRMState, Behavior) -> IO (NRMState, Behavior)
mayLog st =
  return . \case
    Left e -> (st, Log e)
    Right x -> x

passiveSensorBehavior
  :: Cfg.Cfg
  -> NRMState
  -> CPD.SensorID
  -> Double
  -> Maybe (NRMState, UPub.Pub)
passiveSensorBehavior _cfg st sensorID value =
  lookupPassiveSensor sensorID st <&> \Sensor.PassiveSensor {..} ->
    CPD.measure (cpd st) value sensorID & \case
      CPD.Measured m -> (st, UPub.PubMeasurements (CPD.Measurements [m]))
      CPD.NoSuchSensor -> panic "NRM bug: Internal CPD/NRMState coherency error"
      CPD.AdjustProblem r p ->
        ( adjustSensorRange
            sensorID
            r $
            st {cpd = p}
        , UPub.PubCPD p
        )

respondContent :: Text -> Cmd -> CmdID -> URep.OutputType -> Behavior
respondContent content cmd cmdID outputType =
  mayRep cmd $
    outputType & \case
    URep.StdoutOutput ->
      URep.RepStdout $ URep.Stdout
        { URep.stdoutCmdID = cmdID
        , stdoutPayload = content
        }
    URep.StderrOutput ->
      URep.RepStderr $ URep.Stderr
        { URep.stderrCmdID = cmdID
        , stderrPayload = content
        }
