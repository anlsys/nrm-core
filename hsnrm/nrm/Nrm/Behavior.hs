{-|
Module      : Nrm.Behavior
Copyright   : (c) UChicago Argonne, 2019
License     : BSD3
Maintainer  : fre@freux.fr
-}
module Nrm.Behavior
  ( behavior
  , Behavior (..)
  , CmdStatus (..)
  , NrmEvent (..)
  )
where

import qualified Data.Map as DM
import Data.MessagePack
import qualified Nrm.Classes.Messaging as M
import Nrm.NrmState
import qualified Nrm.Types.Configuration as Cfg
import qualified Nrm.Types.Container as C
import Nrm.Types.Messaging.DownstreamEvent as DEvent
import qualified Nrm.Types.Messaging.UpstreamPub as UPub
import qualified Nrm.Types.Messaging.UpstreamRep as URep
import qualified Nrm.Types.Messaging.UpstreamReq as UReq
import Nrm.Types.NrmState
import Nrm.Types.Process
import qualified Nrm.Types.UpstreamClient as UC
import Protolude

data CmdStatus = Launched | NotLaunched
  deriving (Generic, MessagePack)

data NrmEvent
  = Req UC.UpstreamClientID UReq.Req
  | RegisterCmd CmdID CmdStatus
  | Event DEvent.Event
  | DoSensor
  | DoControl
  | DoShutdown
  | DoChildren

data Behavior
  = NoBehavior
  | Rep UC.UpstreamClientID URep.Rep
  | Pub UPub.Pub
  | StartChild CmdID Command Arguments Env
  | KillChildren [CmdID]
  deriving (Generic)

behavior :: Cfg.Cfg -> NrmState -> NrmEvent -> IO (NrmState, Behavior)
behavior _ st (RegisterCmd cmdID cmdstatus) = case cmdstatus of
  NotLaunched -> return (registerFailed cmdID st, NoBehavior)
  Launched -> return (registerLaunched cmdID st, NoBehavior)
behavior _ st (Event msg) = case msg of
  DEvent.ThreadStart _ -> return (st, NoBehavior)
  DEvent.ThreadProgress _ _ -> return (st, NoBehavior)
  DEvent.ThreadPhaseContext _ _ -> return (st, NoBehavior)
  DEvent.ThreadExit _ -> return (st, NoBehavior)
  DEvent.CmdStart _ -> return (st, NoBehavior)
  DEvent.CmdPerformance _ _ -> return (st, NoBehavior)
  DEvent.CmdExit _ -> return (st, NoBehavior)
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
    let st' = registerAwaiting cmdID (mkCmd spec) runContainerID . registerContainer runContainerID $ st
    return (st', StartChild cmdID (cmd spec) (args spec) (env spec))
  UReq.ReqKill UReq.Kill {..} -> do
    let st' = removeContainer killContainerID st
    let cmds = getCmds st killContainerID
    return (st', KillChildren cmds)
  UReq.ReqSetPower _ -> return (st, NoBehavior)
behavior _ st DoSensor = return (st, NoBehavior)
behavior _ st DoControl = return (st, NoBehavior)
behavior _ st DoShutdown = return (st, NoBehavior)
behavior _ st DoChildren = return (st, NoBehavior)

-- | The sensitive tag that has to be pattern-matched on the python side.
instance MessagePack Behavior where

  toObject NoBehavior = toObject ("noop" :: Text)
  toObject (Rep clientid msg) = toObject ("reply" :: Text, clientid, M.encodeT msg)
  toObject (Pub msg) = toObject ("publish" :: Text, msg)
  toObject (StartChild cmdID cmd args env) = toObject ("cmd" :: Text, cmdID, cmd, args, env)
  toObject (KillChildren cmdIDs) = toObject ("kill" :: Text, cmdIDs)

  fromObject x = to <$> gFromObject x

-- | get all Cmds IDs for a container ID
getCmds :: NrmState -> C.ContainerID -> [CmdID]
getCmds st containerID = case DM.lookup containerID (containers st) of
  Nothing -> panic "containerID not found"
  Just c -> DM.keys $ C.cmds c

-- | Removes a container from the state
removeContainer :: C.ContainerID -> NrmState -> NrmState
removeContainer containerID st =
  st {containers = DM.delete containerID (containers st)}

-- | Registers a container if not already tracked in the state, and returns the new state.
registerContainer :: C.ContainerID -> NrmState -> NrmState
registerContainer containerID st =
  case DM.lookup containerID (containers st) of
    Nothing -> st {containers = containers'}
      where
        containers' = DM.insert containerID C.emptyContainer (containers st)
    Just _ -> st

-- | Registers an awaiting command in an existing container
registerAwaiting :: CmdID -> Cmd -> C.ContainerID -> NrmState -> NrmState
registerAwaiting cmdID cmdValue containerID st =
  st {containers = DM.update f containerID (containers st)}
  where
    f c = Just $ c {C.awaiting = DM.insert cmdID cmdValue (C.awaiting c)}

{-{ C.awaiting = DM.delete cmdID (awaiting container)-}
{-, C.cmds = DM.insert cmdID c (cmds container)-}

-- | Turns an awaiting command to a launched one.
registerLaunched :: CmdID -> NrmState -> NrmState
registerLaunched cmdID st =
  case DM.lookup cmdID (awaitingCmdIDContainerIDMap st) of
    Nothing -> panic "internal nrm.so lookup error."
    Just containerID -> case DM.lookup containerID (containers st) of
      Nothing -> panic "container was deleted while command was registering"
      Just container -> case DM.lookup cmdID (C.awaiting container) of
        Nothing -> panic "internal nrm.so lookup error"
        Just cmdValue ->
          st
            { containers = DM.insert containerID
                ( container
                  { C.cmds = DM.insert cmdID cmdValue (C.cmds container)
                  , C.awaiting = DM.delete cmdID (C.awaiting container)
                  }
                )
                (containers st)
            }

-- | Fails an awaiting command.
registerFailed :: CmdID -> NrmState -> NrmState
registerFailed cmdID st =
  case DM.lookup cmdID (awaitingCmdIDContainerIDMap st) of
    Nothing ->
      panic $ "pynrm/nrm.so interaction error: " <>
        "command was not registered as awaiting in any container"
    Just containerID -> st {containers = DM.update f containerID (containers st)}
  where
    f c =
      if null (C.cmds c)
      then Nothing
      else Just $ c {C.awaiting = DM.delete cmdID (C.awaiting c)}
