{-# LANGUAGE TypeApplications #-}

-- |
-- Module      : NRM.Behavior
-- Copyright   : (c) UChicago Argonne, 2019
-- License     : BSD3
-- Maintainer  : fre@freux.fr
module NRM.Behavior
  ( -- * external interface
    behavior,

    -- * internal dispatching
    nrm,
    nrmDownstreamEvent,
    doControl,
    commonSP,
    commonProcess,
    folder,
    mayInjectLibnrmPreload,
    mayRep,
    respondContent,
  )
where

import CPD.Core as CPD
import CPD.Utils as CPD
import CPD.Values as CPD
import qualified Control.Exception.Enclosed as EE
import Control.Lens hiding (to)
import Control.Monad.Trans.RWS.Lazy (RWST)
import Data.Generics.Product
import LMap.Map as LM
import LensMap.Core as LensMap
import qualified NRM.CPD as NRMCPD
import NRM.Control
import NRM.Messaging
import NRM.Sensors as Sensors
import NRM.State
import NRM.Types.Actuator as A
import NRM.Types.Behavior
import NRM.Types.Cmd
import NRM.Types.CmdID as CmdID
import NRM.Types.Configuration as Cfg
import NRM.Types.Controller as Controller
import qualified NRM.Types.DownstreamClient as DC
import NRM.Types.DownstreamCmdID as DCmID
import NRM.Types.DownstreamThreadID as DTID
import NRM.Types.Manifest as Manifest
import qualified NRM.Types.Messaging.DownstreamEvent as DEvent
import qualified NRM.Types.Messaging.UpstreamPub as UPub
import qualified NRM.Types.Messaging.UpstreamRep as URep
import qualified NRM.Types.Messaging.UpstreamReq as UReq
import NRM.Types.NRM
import NRM.Types.Process as Process
import NRM.Types.Sensor as Sensor
import qualified NRM.Types.Slice as Ct
import NRM.Types.State
import qualified NRM.Types.Units as U
import NRM.Types.UpstreamClient
import Protolude hiding (Map, log)

-- | External interface for NRM behavior, without RWS monad
behavior :: Cfg.Cfg -> NRMState -> U.Time -> NRMEvent -> IO (NRMState, [Behavior])
behavior cfg st time event = execNRM (nrm time event) cfg st

-- | The nrm function contains the main logic of the NRM daemon. It changes
-- the state and produces an associated behavior to be executed by the
-- runtime. This contains the slice management logic, the sensor callback
-- logic, the control loop callback logic. Works in the @NRM monad.
nrm :: U.Time -> NRMEvent -> NRM ()
nrm _callTime (DoOutput cmdID outputType content) =
  behaveLens $ _cmdID cmdID go
  where
    go Nothing = (Log Error "No such command was found in the NRM state.", Nothing)
    go (Just c) = case content of
      "" ->
        let newPstate = case outputType of
              URep.StdoutOutput -> (processState c) {stdoutFinished = True}
              URep.StderrOutput -> (processState c) {stderrFinished = True}
         in isDone newPstate & \case
              Just exc ->
                ( mayRep c (URep.RepCmdEnded $ URep.CmdEnded exc),
                  Nothing
                )
              Nothing ->
                ( Log
                    Debug
                    ( "warning, empty content: "
                        <> show content
                    ),
                  Just (c & field @"processState" .~ newPstate)
                )
      _ -> (respondContent content c cmdID outputType, Just c)
nrm _callTime (RegisterCmd cmdID cmdstatus) = do
  st <- get
  case cmdstatus of
    NotLaunched -> case registerFailed cmdID st of
      Just (st', _, _, cmdCore) -> case upstreamClientID cmdCore of
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
nrm _callTime (Req clientid msg) = sendExceptionsUp clientid $ do
  st <- get
  c <- ask
  msg & \case
    UReq.ReqCPD _ ->
      rep
        clientid
        $ URep.RepCPD
        $ NRMCPD.toCPD (controlCfg c) st
    UReq.ReqSliceList _ -> rep clientid (URep.RepList . URep.SliceList . LM.toList $ slices st)
    UReq.ReqGetState _ -> rep clientid (URep.RepGetState st)
    UReq.ReqGetConfig _ -> rep clientid (URep.RepGetConfig $ URep.GetConfig c)
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
      behave
        $ StartChild cmdID runCmd runArgs
        $ (Env $ env spec & fromEnv & LM.insert cmdIDEnvVar (CmdID.toText cmdID))
          & mayInjectLibnrmPreload c manifest
    UReq.ReqKillSlice UReq.KillSlice {..} ->
      behaveLens $ _sliceID killSliceID go
      where
        go Nothing = (Rep clientid $ URep.RepNoSuchSlice URep.NoSuchSlice, Nothing)
        go (Just slice) =
          ( KillChildren (LM.keys $ Ct.cmds slice) $
              (clientid, URep.RepSliceKilled (URep.SliceKilled killSliceID))
                : catMaybes
                  ( (upstreamClientID . cmdCore <$> LM.elems (Ct.cmds slice))
                      <&> fmap (,URep.RepThisCmdKilled URep.ThisCmdKilled)
                  ),
            Nothing
          )
    UReq.ReqActuate actions -> do
      cpd <- NRMCPD.toCPD <$> (ask <&> controlCfg) <*> pure st
      for_ actions $ \action@(Action actuatorID (CPD.DiscreteDouble value)) -> validateAction cpd action & \case
        UnknownActuator -> do
          log "NRM controller received an action request on a non-existent actuator"
          rep clientid (URep.RepActuate URep.NotActuated)
        InvalidAction -> do
          log "NRM controller received an action request on an invalid action"
          rep clientid (URep.RepActuate URep.NotActuated)
        ActionOk ->
          fromCPDKey actuatorID & \case
            Nothing -> log "couldn't decode actuatorID"
            Just aKey -> LM.lookup aKey (lenses st) & \case
              Nothing -> log "NRM internal error: actuator not found, after CPD validation."
              Just (ScopedLens l) -> do
                liftIO $ go (st ^. l) value
                log $ "NRM controller takes action:" <> show value <> " for actuator" <> show actuatorID
                rep clientid (URep.RepActuate URep.Actuated)
    UReq.ReqKillCmd UReq.KillCmd {..} ->
      removeCmd (KCmdID killCmdID) st & \case
        Nothing -> rep clientid $ URep.RepNoSuchCmd URep.NoSuchCmd
        Just (info, _, cmd, sliceID, st') -> do
          put st'
          behave
            $ KillChildren
              [killCmdID]
            $ ( clientid,
                info & \case
                  CmdRemoved -> URep.RepCmdKilled (URep.CmdKilled killCmdID)
                  SliceRemoved -> URep.RepSliceKilled (URep.SliceKilled sliceID)
              )
              : maybe
                []
                (\x -> [(x, URep.RepThisCmdKilled URep.ThisCmdKilled)])
                (upstreamClientID . cmdCore $ cmd)
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
                behave $
                  ClearChild
                    cmdID
                    ( (,URep.RepCmdEnded (URep.CmdEnded exitcode))
                        <$> (upstreamClientID . cmdCore $ cmd)
                    )
              Nothing -> log "Error during command removal from NRM state"
            Nothing -> put $ insertSlice sliceID (Ct.insertCmd cmdID cmd {processState = newPstate} slice) st
nrm callTime (DownstreamEvent clientid msg) =
  nrmDownstreamEvent callTime clientid msg
    <&> ( \case
            OOk m -> Event callTime [m]
            ONotFound -> NoEvent callTime
            OAdjustment -> Reconfigure callTime
        )
    >>= doControl
nrm callTime DoControl = doControl (NoEvent callTime)
nrm callTime DoSensor = do
  st <- get
  (st', measurements) <- lift (foldM (folder callTime) (st, Just []) (LM.toList $ lenses st))
  put st'
  measurements & \case
    Just [] ->
      doControl (Event callTime [])
    Just ms ->
      pub (UPub.PubMeasurements callTime ms)
        >> doControl (Event callTime ms)
    Nothing -> doControl (Reconfigure callTime)
nrm _callTime DoShutdown = return ()

-- | doControl checks the integrator state and triggers a control iteration if NRM is ready.
doControl :: Controller.Input -> NRM ()
doControl input = do
  st <- get
  mccfg <- ask <&> controlCfg
  zoom (field @"controller" . _Just) $ do
    logInfo ("Control input:" <> show input)
    mccfg & \case
      FixedCommand _ -> return ()
      ccfg@ControlCfg {} ->
        let cpd = NRMCPD.toCPD ccfg st
            mRefActions =
              if [] /= CPD.constraints cpd
                then Just $
                  (LM.toList (lenses st) :: [(ActuatorKey, ScopedLens NRMState A.Actuator)])
                    <&> \(k, ScopedLens l) -> CPD.Action
                      { actuatorID = toS k,
                        actuatorValue = CPD.DiscreteDouble $ st ^. (l . field @"referenceAction")
                      }
                else Nothing
         in banditCartesianProductControl ccfg cpd input mRefActions >>= \case
              DoNothing -> return ()
              Decision actions decisionMeta -> do
                get >>= \ctrl -> pub (UPub.PubAction (getTime input) actions decisionMeta ctrl)
                forM_ actions $ \(Action actuatorID (CPD.DiscreteDouble discreteValue)) ->
                  fromCPDKey actuatorID & \case
                    Nothing -> log "couldn't decode actuatorID"
                    Just aKey ->
                      LM.lookup aKey (lenses st) & \case
                        Nothing -> log "NRM internal error: actuator not found."
                        Just (ScopedLens l) -> do
                          liftIO $ go (st ^. l) discreteValue
                          log $ "NRM controller takes action:" <> show discreteValue <> " for actuator" <> show actuatorID
  where
    getTime (Controller.Event t _) = t
    getTime (Controller.NoEvent t) = t
    getTime (Controller.Reconfigure t) = t

-- | Downstream event handler.
nrmDownstreamEvent ::
  U.Time ->
  DC.DownstreamClientID ->
  DEvent.Event ->
  NRM (CommonOutcome CPD.Measurement)
nrmDownstreamEvent callTime clientid = \case
  DEvent.CmdPerformance cmdID perf -> DCmID.fromText (toS clientid) & \case
    Nothing -> log "couldn't decode clientID to UUID" >> return ONotFound
    Just downstreamCmdID -> commonSP
      callTime
      (Sensor.DownstreamCmdKey downstreamCmdID)
      (U.fromOps perf & fromIntegral)
      >>= \case
        ONotFound -> zoom (_cmdID cmdID) $
          get >>= \case
            Nothing -> do
              log $
                "No command was found in the NRM state for "
                  <> "the cmdID associatid with this cmdPerf message."
              return ONotFound
            Just c -> do
              put $ addDownstreamCmdClient c downstreamCmdID
              log "downstream thread registered."
              return OAdjustment
        OAdjustment -> return OAdjustment
        OOk m -> pub (UPub.PubPerformance callTime cmdID perf) >> return (OOk m)
  DEvent.ThreadProgress downstreamThreadID payload ->
    commonSP
      callTime
      (Sensor.DownstreamThreadKey downstreamThreadID)
      (payload & U.fromProgress & fromIntegral)
      >>= \case
        ONotFound -> zoom (_cmdID (cmdID downstreamThreadID)) $
          get >>= \case
            Nothing -> do
              log $
                "No command was found in the NRM state for "
                  <> "this downstreamThreadID."
              return ONotFound
            Just c -> registerDTT c downstreamThreadID
        OAdjustment -> return OAdjustment
        OOk m ->
          pub (UPub.PubProgress callTime downstreamThreadID payload)
            >> return (OOk m)
  DEvent.ThreadPhaseContext downstreamThreadID phaseContext ->
    commonSP
      callTime
      (Sensor.DownstreamThreadKey downstreamThreadID)
      (DEvent.computetime phaseContext & fromIntegral)
      >>= \case
        ONotFound -> zoom (_cmdID (cmdID downstreamThreadID)) $
          get >>= \case
            Nothing -> do
              log $
                "No command was found in the NRM state for"
                  <> " this downstreamThreadID."
              return ONotFound
            Just c -> registerDTT c downstreamThreadID
        OAdjustment -> return OAdjustment
        OOk m -> do
          st <- get
          LM.lookup (cmdID downstreamThreadID) (cmdIDMap st) & \case
            Nothing ->
              log "internal NRM state warning: phasecontext pub lookup failed"
                >> return (OOk m)
            Just (_, sliceID, _) ->
              pub
                ( UPub.PubPhaseContext
                    callTime
                    downstreamThreadID
                    sliceID
                    phaseContext
                )
                >> return (OOk m)
  DEvent.CmdPause cmdID -> DCmID.fromText (toS clientid) & \case
    Nothing -> log "couldn't decode clientID to UUID" >> return ONotFound
    Just downstreamCmdID -> zoom (_cmdID cmdID) $
      get >>= \case
        Nothing -> do
          log $
            "No corresponding command for this downstream"
              <> " cmd 'pause' request."
          return ONotFound
        Just c -> do
          put $
            Just
              ( c & field @"downstreamCmds"
                  . at downstreamCmdID .~ Nothing
              )
          log "downstream cmd un-registered."
          return OAdjustment
  DEvent.ThreadPause downstreamThreadID -> zoom
    (_cmdID (cmdID downstreamThreadID))
    $ get >>= \case
      Nothing ->
        log
          ( "No corresponding command for this downstream thread 'pause'"
              <> " request."
          )
          >> return ONotFound
      Just c -> do
        put $
          Just
            ( c & field @"downstreamThreads"
                . at downstreamThreadID .~ Nothing
            )
        log "downstream thread un-registered."
        return ONotFound
  DEvent.ThreadPhasePause _ ->
    log "unimplemented ThreadPhasePause handler"
      >> return ONotFound
  where
    registerDTT ::
      Cmd ->
      DownstreamThreadID ->
      RWST Cfg [Behavior] (Maybe Cmd) IO (CommonOutcome a)
    registerDTT c dtid = do
      put $ addDownstreamThreadClient c dtid
      log "downstream thread registered."
      return OAdjustment

data CommonOutcome a = OAdjustment | OOk a | ONotFound

commonSP ::
  U.Time ->
  ActiveSensorKey ->
  Double ->
  NRM (CommonOutcome Measurement)
commonSP callTime key value = do
  cfg <- ask
  st <- get
  processActiveSensor cfg callTime st key value & commonProcess callTime

commonProcess :: U.Time -> MeasurementOutput -> NRM (CommonOutcome Measurement)
commonProcess callTime = \case
  Sensors.NotFound -> return ONotFound
  Sensors.Adjusted st' -> do
    put st'
    logInfo "Out-of-range value received, sensor adjusted."
    return OAdjustment
  Sensors.Ok st' measurement -> do
    put st'
    pub $ UPub.PubMeasurements callTime [measurement]
    return $ OOk measurement

mayRep :: Cmd -> URep.Rep -> Behavior
mayRep c rp =
  (upstreamClientID . cmdCore) c & \case
    Just ucID -> Rep ucID rp
    Nothing ->
      Log
        Error
        ( "This command does not have a registered"
            <> " upstream client."
        )

respondContent :: Text -> Cmd -> CmdID -> URep.OutputType -> Behavior
respondContent content cmd cmdID outputType = mayRep cmd $
  outputType & \case
    URep.StdoutOutput ->
      URep.RepStdout $ URep.Stdout {URep.stdoutCmdID = cmdID, stdoutPayload = content}
    URep.StderrOutput ->
      URep.RepStderr $ URep.Stderr {URep.stderrCmdID = cmdID, stderrPayload = content}

folder ::
  U.Time ->
  (NRMState, Maybe [CPD.Measurement]) -> -- snd = Nothing <=> cpd changed.
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
        env & LM.insert ratelimitEnvVar (show $ U.fromHz ratelimit)
          & LM.alter
            ( \case
                Nothing -> Just path
                Just x -> Just $ x <> " " <> path
            )
            "LD_PRELOAD"

behaveLens :: (NRMState -> (Behavior, NRMState)) -> NRM ()
behaveLens l = do
  st <- get
  l st & \(bh, st') -> put st' >> behave bh

sendExceptionsUp :: UpstreamClientID -> NRM () -> NRM ()
sendExceptionsUp clientid nrmMonad = exceptionLess nrmMonad >>= \case
  Left content -> do
    logError content
    rep clientid (URep.RepException content)
  Right _ -> return ()

exceptionLess :: NRM a -> NRM (Either Text a)
exceptionLess io = EE.catchAny (Right <$> io) $ return . Left . show
