{-# LANGUAGE TypeApplications #-}

-- |
-- Module      : NRM.Behavior
-- Copyright   : (c) UChicago Argonne, 2019
-- License     : BSD3
-- Maintainer  : fre@freux.fr
module NRM.Behavior
  ( -- * NRM's core logic.
    behavior,
  )
where

import CPD.Values as CPD
import Control.Lens hiding (to)
import Data.Generics.Product
import Data.Map as DM
import LMap.Map as LM
import LensMap.Core as LensMap
import qualified NRM.CPD as NRMCPD
import NRM.Sensors as Sensors
import NRM.State
import NRM.Types.Behavior
import NRM.Types.Cmd
import NRM.Types.CmdID as CmdID
import NRM.Types.Configuration as Cfg
import qualified NRM.Types.DownstreamThreadID as DTID
import NRM.Types.Manifest as Manifest
import qualified NRM.Types.Messaging.DownstreamEvent as DEvent
import qualified NRM.Types.Messaging.UpstreamPub as UPub
import qualified NRM.Types.Messaging.UpstreamRep as URep
import qualified NRM.Types.Messaging.UpstreamReq as UReq
import NRM.Types.Process as Process
import NRM.Types.Sensor as Sensor
import qualified NRM.Types.Slice as Ct
import NRM.Types.State
import qualified NRM.Types.Units as U
import qualified NRM.Types.UpstreamClient as UC
import Protolude

mayRep :: Cmd -> URep.Rep -> Behavior
mayRep c rep =
  (upstreamClientID . cmdCore) c & \case
    Just ucID -> Rep ucID rep
    Nothing -> Log "This command does not have a registered upstream client."

-- | The behavior function contains the main logic of the NRM daemon. It changes the state and
-- produces an associated behavior to be executed by the runtime. This contains the slice
-- management logic, the sensor callback logic, the control loop callback logic.
behavior :: Cfg.Cfg -> NRMState -> U.Time -> NRMEvent -> IO (NRMState, Behavior)
behavior _ st _callTime (DoOutput cmdID outputType content) =
  return . swap $
    st
      & _cmdID
        cmdID
        ( \case
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
        )
behavior _ st _callTime (RegisterCmd cmdID cmdstatus) =
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
              ( st',
                Rep clientID (URep.RepStart (URep.Start sliceID cmdID))
              )
behavior c st _callTime (Req clientid msg) =
  msg & \case
    UReq.ReqCPD _ -> justReply st clientid . URep.RepCPD . URep.CPD $ NRMCPD.toCPD st
    UReq.ReqSliceList _ -> bhv st . Rep clientid . URep.RepList . URep.SliceList . LM.toList $ slices st
    UReq.ReqGetState _ -> bhv st . Rep clientid . URep.RepGetState $ URep.GetState st
    UReq.ReqGetConfig _ -> bhv st . Rep clientid . URep.RepGetConfig $ URep.GetConfig c
    UReq.ReqRun UReq.Run {..} -> do
      cmdID <- CmdID.nextCmdID <&> fromMaybe (panic "couldn't generate next cmd id")
      let (runCmd, runArgs) =
            (cmd spec, args spec)
              & ( if Manifest.perfwrapper (Manifest.app manifest)
                    /= Manifest.PerfwrapperDisabled
                    then wrapCmd (Cfg.argo_perf_wrapper c)
                    else identity
                )
      return
        ( registerAwaiting
            cmdID
            ( mkCmd
                spec
                manifest
                ( if detachCmd
                    then Nothing
                    else Just clientid
                )
            )
            runSliceID
            . createSlice runSliceID
            $ st,
          StartChild cmdID runCmd runArgs $
            ( Env $
                env spec & fromEnv
                  & LM.insert "NRM_CMDID" (toText cmdID)
            )
              & mayInjectLibnrmPreload c manifest
        )
    UReq.ReqKillSlice UReq.KillSlice {..} -> do
      let (maybeSlice, st') = removeSlice killSliceID st
      return
        ( st',
          fromMaybe
            (Rep clientid $ URep.RepNoSuchSlice URep.NoSuchSlice)
            ( maybeSlice <&> \slice ->
                KillChildren (LM.keys $ Ct.cmds slice) $
                  (clientid, URep.RepSliceKilled (URep.SliceKilled killSliceID))
                    : catMaybes
                      ( (upstreamClientID . cmdCore <$> LM.elems (Ct.cmds slice))
                          <&> fmap (,URep.RepThisCmdKilled URep.ThisCmdKilled)
                      )
            )
        )
    UReq.ReqSetPower _ -> return (st, NoBehavior)
    UReq.ReqKillCmd UReq.KillCmd {..} ->
      removeCmd (KCmdID killCmdID) st & \case
        Nothing -> bhv st $ Rep clientid (URep.RepNoSuchCmd URep.NoSuchCmd)
        Just (info, _, cmd, sliceID, st') ->
          bhv st'
            $ KillChildren [killCmdID]
            $ ( clientid,
                info & \case
                  CmdRemoved -> URep.RepCmdKilled (URep.CmdKilled killCmdID)
                  SliceRemoved -> URep.RepSliceKilled (URep.SliceKilled sliceID)
              )
              : maybe
                []
                (\x -> [(x, URep.RepThisCmdKilled URep.ThisCmdKilled)])
                (upstreamClientID . cmdCore $ cmd)
behavior _ st _callTime (ChildDied pid exitcode) =
  lookupProcess pid st & \case
    Nothing -> bhv st $ Log "No such PID in NRM's state."
    Just (cmdID, cmd, sliceID, slice) ->
      let newPstate = (processState cmd) {ended = Just exitcode}
       in isDone newPstate & \case
            Just _ ->
              removeCmd (KProcessID pid) st & \case
                Just (_, _, _, _, st') ->
                  bhv st' $
                    ClearChild
                      cmdID
                      ( (,URep.RepCmdEnded (URep.CmdEnded exitcode))
                          <$> (upstreamClientID . cmdCore $ cmd)
                      )
                Nothing -> bhv st $ panic "Error during command removal from NRM state"
            Nothing ->
              noBhv $
                insertSlice
                  sliceID
                  (Ct.insertCmd cmdID cmd {processState = newPstate} slice)
                  st
behavior _ st _callTime (DoControl _time) = bhv st NoBehavior
behavior _ st _callTime (DoSensor time) = do
  -- This function is complicated, needs custom types.
  (st', measurements) <- foldM (folder time) (st, Just []) (DM.toList lMap)
  measurements & \case
    Just ms -> bhv st' $ Pub [UPub.PubMeasurements time ms]
    Nothing -> bhv st' $ Pub [UPub.PubCPD time (NRMCPD.toCPD st')]
  where
    lMap :: LensMap NRMState PassiveSensorKey PassiveSensor
    lMap = lenses st
behavior _ st _callTime DoShutdown = bhv st NoBehavior
behavior cfg st callTime (DownstreamEvent clientid msg) =
  msg & \case
    DEvent.ThreadProgress _ _ -> return (st, Log "unimplemented")
    DEvent.ThreadPhaseContext downstreamThreadID phaseContext ->
      Sensors.process
        cfg
        callTime
        st
        (Sensor.DownstreamThreadKey downstreamThreadID)
        (DEvent.computetime phaseContext & fromIntegral)
        & \case
          Sensors.NotFound ->
            return $
              registerDownstreamCmdClient (DTID.cmdID downstreamThreadID) clientid st
                <&> (,Log "downstream cmd client registered.")
                & fromMaybe (st, Log "no such Cmd")
          Sensors.Adjusted st' -> bhv st' $ Log "Out-of-range value received, sensor adjusted"
          Sensors.Ok st' measurement ->
            bhv st' $
              Pub
                [ UPub.PubMeasurements callTime [measurement],
                  UPub.PubPhaseContext callTime downstreamThreadID phaseContext
                ]
    DEvent.ThreadPause _ -> return (st, Log "unimplemented ThreadPause handler")
    DEvent.ThreadPhasePause _ -> return (st, Log "unimplemented ThreadPhasePause handler")
    DEvent.CmdPerformance cmdID perf ->
      Sensors.process
        cfg
        callTime
        st
        (Sensor.DownstreamCmdKey clientid)
        (U.fromOps perf & fromIntegral)
        & \case
          Sensors.NotFound ->
            return $
              registerDownstreamCmdClient cmdID clientid st
                <&> (,Log "downstream cmd client registered.")
                & fromMaybe (st, Log "no such Cmd")
          Sensors.Adjusted st' -> bhv st' $ Log "Out-of-range value received, sensor adjusted"
          Sensors.Ok st' measurement ->
            bhv st' $
              Pub
                [ UPub.PubMeasurements callTime [measurement],
                  UPub.PubPerformance callTime cmdID perf
                ]
    DEvent.CmdPause cmdID ->
      unRegisterDownstreamCmdClient cmdID clientid st & \case
        Just st' -> bhv st' $ Log "downstream cmd client un-registered."
        Nothing -> bhv st $ Log "No corresponding command for this downstream cmd 'pause' request."

noBhv :: a -> IO (a, Behavior)
noBhv = flip bhv NoBehavior

bhv :: Monad m => a -> b -> m (a, b)
bhv st x = return (st, x)

justReply :: Monad m => a -> UC.UpstreamClientID -> URep.Rep -> m (a, Behavior)
justReply st clientID = bhv st . Rep clientID

mayLog :: NRMState -> Either Text (NRMState, Behavior) -> IO (NRMState, Behavior)
mayLog st =
  return . \case
    Left e -> (st, Log e)
    Right x -> x

respondContent :: Text -> Cmd -> CmdID -> URep.OutputType -> Behavior
respondContent content cmd cmdID outputType =
  mayRep cmd $
    outputType & \case
      URep.StdoutOutput ->
        URep.RepStdout $ URep.Stdout
          { URep.stdoutCmdID = cmdID,
            stdoutPayload = content
          }
      URep.StderrOutput ->
        URep.RepStderr $ URep.Stderr
          { URep.stderrCmdID = cmdID,
            stderrPayload = content
          }

folder ::
  U.Time ->
  (NRMState, Maybe [CPD.Measurement]) -> -- if second value is Nothing, cpd changed.
  (PassiveSensorKey, ScopedLens NRMState PassiveSensor) ->
  IO (NRMState, Maybe [CPD.Measurement])
folder time (s, ms) (k, ScopedLens l) =
  let ps = view l s
   in perform ps <&> \case
        Just value ->
          processPassiveSensor ps time (toS k) value & \case
            LegalMeasurement sensor' measurement ->
              ms & \case
                Nothing -> (s & l .~ sensor', Nothing)
                Just msValues ->
                  ( s & l .~ sensor',
                    Just $ measurement : msValues
                  )
            IllegalValueRemediation sensor' -> (s & l .~ sensor', Nothing)
        Nothing ->
          processPassiveSensorFailure ps time & \case
            LegalFailure -> (s, ms)
            IllegalFailureRemediation sensor' -> (s & l .~ sensor', Nothing)

mayInjectLibnrmPreload :: Cfg -> Manifest -> Env -> Env
mayInjectLibnrmPreload c manifest e =
  fromMaybe e $
    injector
      <$> ((Manifest.instrumentation . Manifest.app) manifest <&> Manifest.ratelimit)
      <*> Cfg.libnrmPath c
      <*> Just e
  where
    injector :: U.Frequency -> Text -> Env -> Env
    injector ratelimit path (Env env) =
      Env $
        env & LM.insert "NRM_RATELIMIT" (show $ U.fromHz ratelimit)
          & LM.alter
            ( \case
                Nothing -> Just path
                Just x -> Just $ x <> " " <> path
            )
            "LD_PRELOAD"
