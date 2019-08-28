{-|
Module      : Nrm.Behavior
Copyright   : (c) UChicago Argonne, 2019
License     : BSD3
Maintainer  : fre@freux.fr
-}
module Nrm.Behavior
  ( -- * Nrm's core logic.
    behavior
  , -- * The Event specification
    NrmEvent (..)
  , -- * The Behavior specification
    Behavior (..)
  , CmdStatus (..)
  , OutputType (..)
  )
where

import qualified Data.Map as DM
import Data.MessagePack
import qualified Nrm.Classes.Messaging as M
import Nrm.NrmState
import qualified Nrm.Types.Configuration as Cfg
import qualified Nrm.Types.Container as Ct
import Nrm.Types.Messaging.DownstreamEvent as DEvent
import qualified Nrm.Types.Messaging.UpstreamPub as UPub
import qualified Nrm.Types.Messaging.UpstreamRep as URep
import qualified Nrm.Types.Messaging.UpstreamReq as UReq
import Nrm.Types.NrmState
import Nrm.Types.Process
import qualified Nrm.Types.UpstreamClient as UC
import Protolude

-- | The Behavior datatype describes an event from the runtime on which to react.
data NrmEvent
  = -- | A Request was received on the Upstream API.
    Req UC.UpstreamClientID UReq.Req
  | -- | Registering a child process.
    RegisterCmd CmdID CmdStatus
  | -- | Event from the application side.
    DownstreamEvent DEvent.Event
  | -- | Stdin/stdout data from the app side.
    DoOutput CmdID OutputType Text
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

data OutputType = Stdout | Stderr

-- | The Behavior datatype encodes a behavior to be executed by the NRM runtime.
data Behavior
  = -- | The No-Op
    NoBehavior
  | -- | Reply to an upstream client.
    Rep UC.UpstreamClientID URep.Rep
  | -- | Publish a message on upstream
    Pub UPub.Pub
  | -- | Start a child process
    StartChild CmdID Command Arguments Env
  | -- | Kill children processes and send some messages back upstream.
    KillChildren [CmdID] [(UC.UpstreamClientID, URep.Rep)]
  | -- | Pop one child process and may send a message back upstream.
    ClearChild CmdID (Maybe (UC.UpstreamClientID, URep.Rep))
  deriving (Generic)

-- | The behavior function contains the main logic of the NRM daemon. It changes the state and
-- produces an associated behavior to be executed by the runtime.
behavior :: Cfg.Cfg -> NrmState -> NrmEvent -> IO (NrmState, Behavior)
behavior _ st (DoOutput cmdID outputType content) = do
  let containerID =
        fromMaybe
          ( panic
            "received output for an absent CmdID: Internal nrm state management error."
          ) $
          DM.lookup cmdID (runningCmdIDContainerIDMap st)
  return
    ( st
    , if content == ""
    then NoBehavior
    else
      case lookupCmd cmdID st of
        Just c -> case (upstreamClientID . cmdCore) c of
          Just ucID ->
            Rep ucID $ case outputType of
              Stdout ->
                URep.RepStdout $ URep.Stdout
                  { URep.stdoutContainerID = containerID
                  , stdoutPayload = content
                  }
              Stderr ->
                URep.RepStderr $ URep.Stderr
                  { URep.stderrContainerID = containerID
                  , stderrPayload = content
                  }
          Nothing -> NoBehavior
        Nothing -> NoBehavior
    )
behavior _ st (RegisterCmd cmdID cmdstatus) = case cmdstatus of
  NotLaunched ->
    return $ fromMaybe (st, NoBehavior) $
      registerFailed cmdID st >>= \(st', _, _, cmdCore) ->
      upstreamClientID cmdCore <&> \x ->
        (st', Rep x (URep.RepStartFailure URep.StartFailure))
  Launched pid ->
    return $ case registerLaunched cmdID pid st of
      Just (st', containerID, clientID) ->
        ( st'
        , Rep clientID (URep.RepStart (URep.Start containerID cmdID))
        )
      Nothing -> (st, NoBehavior)
behavior c st (Req clientid msg) = case msg of
  UReq.ReqContainerList _ ->
    return (st, Rep clientid (URep.RepList rep))
    where
      rep = URep.ContainerList (DM.toList (containers st))
  UReq.ReqGetState _ ->
    return (st, Rep clientid (URep.RepGetState (URep.GetState st)))
  UReq.ReqGetConfig _ ->
    return (st, Rep clientid (URep.RepGetConfig (URep.GetConfig c)))
  UReq.ReqRun UReq.Run {..} -> do
    cmdID <- nextCmdID <&> fromMaybe (panic "couldn't generate next cmd id")
    return
      ( registerAwaiting cmdID
          (mkCmd spec (if detachCmd then Nothing else Just clientid))
          runContainerID .
          createContainer runContainerID $
          st
      , StartChild cmdID (cmd spec) (args spec) (env spec)
      )
  UReq.ReqKillContainer UReq.KillContainer {..} -> do
    let (maybeContainer, st') = removeContainer killContainerID st
    return
      ( st'
      , fromMaybe NoBehavior
        ( maybeContainer <&> \container ->
          KillChildren (DM.keys $ Ct.cmds container) $
            (clientid, URep.RepContainerKilled (URep.ContainerKilled killContainerID)) :
            catMaybes
              ( (upstreamClientID . cmdCore <$> DM.elems (Ct.cmds container)) <&>
                fmap (,URep.RepThisCmdKilled URep.ThisCmdKilled)
              )
        )
      )
  UReq.ReqSetPower _ -> return (st, NoBehavior)
  UReq.ReqKillCmd UReq.KillCmd {..} ->
    return $ fromMaybe (st, NoBehavior) $
      removeCmd (KCmdID killCmdID) st <&> \(info, _, cmd, containerID, st') ->
      ( st'
      , KillChildren [killCmdID] $
        ( clientid
        , case info of
          CmdRemoved -> URep.RepCmdKilled (URep.CmdKilled killCmdID)
          ContainerRemoved -> URep.RepContainerKilled (URep.ContainerKilled containerID)
        ) :
        maybe [] (\x -> [(x, URep.RepThisCmdKilled URep.ThisCmdKilled)]) (upstreamClientID . cmdCore $ cmd)
      )
behavior _ st (ChildDied pid exitcode) =
  return $ case removeCmd (KProcessID pid) st of
    Just (_, cmdID, cmd, _, st') ->
      ( st'
      , ClearChild cmdID
        ( (,URep.RepCmdEnded (URep.CmdEnded exitcode)) <$>
          (upstreamClientID . cmdCore $ cmd)
        )
      )
    Nothing -> (st, NoBehavior)
behavior _ st DoSensor = return (st, NoBehavior)
behavior _ st DoControl = return (st, NoBehavior)
behavior _ st DoShutdown = return (st, NoBehavior)
behavior _ st (DownstreamEvent msg) = case msg of
  DEvent.ThreadStart _ -> return (st, NoBehavior)
  DEvent.ThreadProgress _ _ -> return (st, NoBehavior)
  DEvent.ThreadPhaseContext _ _ -> return (st, NoBehavior)
  DEvent.ThreadExit _ -> return (st, NoBehavior)
  DEvent.CmdStart _ -> return (st, NoBehavior)
  DEvent.CmdPerformance _ _ -> return (st, NoBehavior)
  DEvent.CmdExit _ -> return (st, NoBehavior)

-- | The sensitive unpacking that has to be pattern-matched on the python side.
-- These toObject/fromObject functions do not correspond to each other and the instance
-- just exists for passing the behavior to the python runtime.
instance MessagePack Behavior where

  toObject NoBehavior = toObject ("noop" :: Text)
  toObject (Pub msg) = toObject ("publish" :: Text, M.encodeT msg)
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
      , (\(clientid, msg) -> (clientid, M.encodeT msg)) <$> toList maybeRep
      )

  fromObject x = to <$> gFromObject x
