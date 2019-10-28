{-# LANGUAGE TypeApplications #-}

-- |
-- Module      : NRM.Behavior
-- Copyright   : (c) UChicago Argonne, 2019
-- License     : BSD3
-- Maintainer  : fre@freux.fr
module NRM.Behavior
  ( -- * external interface
    behavior,

    -- * NRM
    nrm,

    -- * basic actions
    rep,
    pub,
    log,
  )
where

import CPD.Values as CPD
import Control.Lens hiding (to)
import Control.Monad.Trans.RWS.Lazy (RWST, execRWST, tell)
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
import qualified NRM.Types.DownstreamClient as DC
import NRM.Types.DownstreamCmdID as DCmID
import NRM.Types.DownstreamThreadID as DTID
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
import Protolude hiding (log)

type NRM = RWST Cfg.Cfg [Behavior] NRMState IO ()

-- | External interface for NRM behavior, without RWS monad
behavior :: Cfg.Cfg -> NRMState -> U.Time -> NRMEvent -> IO (NRMState, [Behavior])
behavior cfg st time event = execRWST (nrm time event) cfg st

-- | The nrm function contains the main logic of the NRM daemon. It changes the state and
-- produces an associated behavior to be executed by the runtime. This contains the slice
-- management logic, the sensor callback logic, the control loop callback logic. RWS monad.
nrm :: U.Time -> NRMEvent -> NRM
nrm _callTime (DoOutput cmdID outputType content) = do
  st <- get
  let (bh, st') = st & _cmdID cmdID \case
        Nothing -> (Log "No such command was found in the NRM state.", Nothing)
        Just c -> content & \case
          "" ->
            let newPstate = outputType & \case
                  URep.StdoutOutput -> (processState c) {stdoutFinished = True}
                  URep.StderrOutput -> (processState c) {stderrFinished = True}
             in isDone newPstate & \case
                  Just exc -> (mayRep c (URep.RepCmdEnded $ URep.CmdEnded exc), Nothing)
                  Nothing -> (NoBehavior, Just (c & field @"processState" .~ newPstate))
          _ -> (respondContent content c cmdID outputType, Just c)
  put st'
  tell [bh]
nrm _callTime (RegisterCmd cmdID cmdstatus) = do
  st <- get
  cmdstatus & \case
    NotLaunched -> registerFailed cmdID st & \case
      Just (st', _, _, cmdCore) ->
        upstreamClientID cmdCore & \case
          Just ucid -> do
            put st'
            rep ucid $ URep.RepStartFailure URep.StartFailure
          Nothing -> return ()
      Nothing -> return ()
    Launched pid -> registerLaunched cmdID pid st & \case
      Left l -> log l
      Right (st', sliceID, maybeClientID) -> do
        put st'
        maybeClientID & \case
          Just clientID -> rep clientID $ URep.RepStart (URep.Start sliceID cmdID)
          Nothing -> return ()
nrm _callTime (Req clientid msg) = do
  st <- get
  c <- ask
  msg & \case
    UReq.ReqCPD _ -> rep clientid $ URep.RepCPD . URep.CPD $ NRMCPD.toCPD st
    UReq.ReqSliceList _ -> rep clientid $ URep.RepList . URep.SliceList . LM.toList $ slices st
    UReq.ReqGetState _ -> rep clientid $ URep.RepGetState $ URep.GetState st
    UReq.ReqGetConfig _ -> rep clientid $ URep.RepGetConfig $ URep.GetConfig c
    UReq.ReqRun UReq.Run {..} -> do
      cmdID <- lift CmdID.nextCmdID <&> fromMaybe (panic "couldn't generate next cmd id")
      let (runCmd, runArgs) =
            (cmd spec, args spec)
              & ( if Manifest.perfwrapper (Manifest.app manifest)
                    /= Manifest.PerfwrapperDisabled
                    then wrapCmd (Cfg.argo_perf_wrapper c)
                    else identity
                )
      modify $
        registerAwaiting
          cmdID
          (mkCmd spec manifest (if detachCmd then Nothing else Just clientid))
          runSliceID
          . createSlice runSliceID
      tell
        [ StartChild cmdID runCmd runArgs $
            (Env $ env spec & fromEnv & LM.insert "NRM_CMDID" (CmdID.toText cmdID))
              & mayInjectLibnrmPreload c manifest
        ]
    UReq.ReqKillSlice UReq.KillSlice {..} -> do
      let (maybeSlice, st') = removeSlice killSliceID st
      put st'
      tell
        [ fromMaybe
            (Rep clientid $ URep.RepNoSuchSlice URep.NoSuchSlice)
            ( maybeSlice <&> \slice ->
                KillChildren (LM.keys $ Ct.cmds slice) $
                  (clientid, URep.RepSliceKilled (URep.SliceKilled killSliceID))
                    : catMaybes
                      ( (upstreamClientID . cmdCore <$> LM.elems (Ct.cmds slice))
                          <&> fmap (,URep.RepThisCmdKilled URep.ThisCmdKilled)
                      )
            )
        ]
    UReq.ReqSetPower _ -> return ()
    UReq.ReqKillCmd UReq.KillCmd {..} ->
      removeCmd (KCmdID killCmdID) st & \case
        Nothing -> rep clientid $ URep.RepNoSuchCmd URep.NoSuchCmd
        Just (info, _, cmd, sliceID, st') -> do
          put st'
          tell $
            [ KillChildren [killCmdID] $
                ( clientid,
                  info & \case
                    CmdRemoved -> URep.RepCmdKilled (URep.CmdKilled killCmdID)
                    SliceRemoved -> URep.RepSliceKilled (URep.SliceKilled sliceID)
                )
                  : maybe
                    []
                    (\x -> [(x, URep.RepThisCmdKilled URep.ThisCmdKilled)])
                    (upstreamClientID . cmdCore $ cmd)
            ]
nrm _callTime (ChildDied pid exitcode) = do
  st <- get
  lookupProcess pid st & \case
    Nothing -> log "No such PID in NRM's state."
    Just (cmdID, cmd, sliceID, slice) ->
      let newPstate = (processState cmd) {ended = Just exitcode}
       in isDone newPstate & \case
            Just _ -> removeCmd (KProcessID pid) st & \case
              Just (_, _, _, _, st') -> do
                put st'
                tell
                  [ ClearChild
                      cmdID
                      ( (,URep.RepCmdEnded (URep.CmdEnded exitcode))
                          <$> (upstreamClientID . cmdCore $ cmd)
                      )
                  ]
              Nothing -> log "Error during command removal from NRM state"
            Nothing -> put $ insertSlice sliceID (Ct.insertCmd cmdID cmd {processState = newPstate} slice) st
nrm callTime (DownstreamEvent clientid msg) = do
  nrmDownstreamEvent callTime clientid msg
  mayControl callTime
nrm callTime DoControl = do
  mayControl callTime
nrm callTime DoSensor = do
  st <- get
  (st', x) <- lift (foldM (folder callTime) (st, Just []) (DM.toList $ lenses st))
  put st'
  x & \case
    Just ms -> pub $ UPub.PubMeasurements callTime ms
    Nothing -> pub $ UPub.PubCPD callTime (NRMCPD.toCPD st')
nrm _callTime DoShutdown = tell [NoBehavior]

-- | nrmControl checks the integrator state and triggers a control iteration if NRM is ready.
mayControl :: p -> a
mayControl _callTime = undefined

nrmDownstreamEvent :: U.Time -> DC.DownstreamClientID -> DEvent.Event -> NRM
nrmDownstreamEvent callTime clientid = \case
  DEvent.CmdPerformance cmdID perf -> DCmID.fromText (toS clientid) & \case
    Nothing -> log "couldn't decode clientID to UUID"
    Just downstreamCmdID -> commonSP callTime (Sensor.DownstreamCmdKey downstreamCmdID) (U.fromOps perf & fromIntegral) >>= \case
      ONotFound -> zoom (_cmdID cmdID) $
        get >>= \case
          Nothing -> log "No command was found in the NRM state for the cmdID associatid with this cmdPerf message."
          Just c -> do
            log "downstream thread registered."
            put $ addDownstreamCmdClient c downstreamCmdID
      OAdjustment -> return ()
      OOk -> pub $ UPub.PubPerformance callTime cmdID perf
  DEvent.ThreadProgress downstreamThreadID payload ->
    commonSP callTime (Sensor.DownstreamThreadKey downstreamThreadID) (payload & U.fromProgress & fromIntegral) >>= \case
      ONotFound -> zoom (_cmdID (cmdID downstreamThreadID)) $
        get >>= \case
          Nothing -> log "No command was found in the NRM state for this downstreamThreadID."
          Just c -> do
            log "downstream thread registered."
            put $ addDownstreamThreadClient c downstreamThreadID
      OAdjustment -> return ()
      OOk -> pub $ UPub.PubProgress callTime downstreamThreadID payload
  DEvent.ThreadPhaseContext downstreamThreadID phaseContext ->
    commonSP callTime (Sensor.DownstreamThreadKey downstreamThreadID) (DEvent.computetime phaseContext & fromIntegral) >>= \case
      ONotFound -> zoom (_cmdID (cmdID downstreamThreadID)) $
        get >>= \case
          Nothing -> log "No command was found in the NRM state for this downstreamThreadID."
          Just c -> do
            log "downstream thread registered."
            put $ addDownstreamThreadClient c downstreamThreadID
      OAdjustment -> return ()
      OOk -> pub $ UPub.PubPhaseContext callTime downstreamThreadID phaseContext
  DEvent.CmdPause cmdID -> DCmID.fromText (toS clientid) & \case
    Nothing -> log "couldn't decode clientID to UUID"
    Just downstreamCmdID -> zoom (_cmdID cmdID) $
      get >>= \case
        Nothing -> log "No corresponding command for this downstream cmd 'pause' request."
        Just c -> do
          put $ Just (c & field @"downstreamCmds" . at downstreamCmdID .~ Nothing)
          log "downstream cmd un-registered."
  DEvent.ThreadPause downstreamThreadID -> zoom (_cmdID (cmdID downstreamThreadID)) $
    get >>= \case
      Nothing -> log "No corresponding command for this downstream thread 'pause' request."
      Just c -> do
        log "downstream thread un-registered."
        put $ Just (c & field @"downstreamThreads" . at downstreamThreadID .~ Nothing)
  DEvent.ThreadPhasePause _ -> log "unimplemented ThreadPhasePause handler"

data CommonOutcome = OAdjustment | OOk | ONotFound

commonSP callTime key value = do
  cfg <- ask
  st <- get
  Sensors.process cfg callTime st key value & commonProcess callTime

commonProcess callTime = \case
  Sensors.NotFound -> return ONotFound
  Sensors.Adjusted st' -> do
    put st'
    log "Out-of-range value received, sensor adjusted"
    return OAdjustment
  Sensors.Ok st' measurement -> do
    put st'
    pub $ UPub.PubMeasurements callTime [measurement]
    return OOk

-- | NRM reply
rep :: UC.UpstreamClientID -> URep.Rep -> NRM
rep clientID rp = tell [Rep clientID rp]

-- | NRM publish
pub :: UPub.Pub -> NRM
pub msg = tell [Pub msg]

-- | NRM log
log :: Text -> RWST Cfg.Cfg [Behavior] a IO ()
log l = tell [Log l]

mayRep :: Cmd -> URep.Rep -> Behavior
mayRep c rp =
  (upstreamClientID . cmdCore) c & \case
    Just ucID -> Rep ucID rp
    Nothing -> Log "This command does not have a registered upstream client."

respondContent :: Text -> Cmd -> CmdID -> URep.OutputType -> Behavior
respondContent content cmd cmdID outputType = mayRep cmd $
  outputType & \case
    URep.StdoutOutput ->
      URep.RepStdout $ URep.Stdout {URep.stdoutCmdID = cmdID, stdoutPayload = content}
    URep.StderrOutput ->
      URep.RepStderr $ URep.Stderr {URep.stderrCmdID = cmdID, stderrPayload = content}

folder ::
  U.Time ->
  (NRMState, Maybe [CPD.Measurement]) -> -- if second value is Nothing, cpd changed.
  (PassiveSensorKey, ScopedLens NRMState PassiveSensor) ->
  IO (NRMState, Maybe [CPD.Measurement])
folder time (s, ms) (k, ScopedLens l) = perform ps <&> doCase
  where
    ps = view l s
    doCase (Just value) = processPassiveSensor ps time (toS k) value & \case
      LegalMeasurement sensor' measurement -> ms & \case
        Nothing -> (s & l .~ sensor', Nothing)
        Just msValues -> (s & l .~ sensor', Just $ measurement : msValues)
      IllegalValueRemediation sensor' -> (s & l .~ sensor', Nothing)
    doCase Nothing = processPassiveSensorFailure ps time & \case
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
