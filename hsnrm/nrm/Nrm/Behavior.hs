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
  , OutputType (..)
  , NrmEvent (..)
  )
where

import qualified Data.Map as DM
import Data.MessagePack
import qualified Nrm.Classes.Messaging as M
import Nrm.NrmState
import qualified Nrm.Types.Configuration as Cfg
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

data OutputType = Stdout | Stderr

data NrmEvent
  = Req UC.UpstreamClientID UReq.Req
  | RegisterCmd UC.UpstreamClientID CmdID CmdStatus
  | DownstreamEvent DEvent.Event
  | DoOutput CmdID OutputType Text
  | DoSensor
  | DoControl
  | DoShutdown
  | DoChildren

data Behavior
  = NoBehavior
  | Rep UC.UpstreamClientID URep.Rep
  | Pub UPub.Pub
  | StartChild UC.UpstreamClientID CmdID Command Arguments Env
  | KillChildren [CmdID]
  deriving (Generic)

behavior :: Cfg.Cfg -> NrmState -> NrmEvent -> IO (NrmState, Behavior)
behavior _ st (DoOutput cmdID outputType content) =
  let containerID =
        fromMaybe
          ( panic
            "received signal for an absent CmdID: Internal nrm state management error."
          ) $
          DM.lookup cmdID (runningCmdIDContainerIDMap st)
   in return
        ( st
        , case lookupCmd cmdID st of
          Just c -> case upstreamClientID c of
            Just ucID ->
              case outputType of
                Stdout ->
                  if content == ""
                  then Rep ucID (URep.RepEndStream URep.EndStream)
                  else
                    Rep ucID $ URep.RepStdout $ URep.Stdout
                      { URep.stdoutContainerID = containerID
                      , stdoutPayload = content
                      }
                Stderr ->
                  if content == ""
                  then NoBehavior
                  else
                    Rep ucID $ URep.RepStderr $ URep.Stderr
                      { URep.stderrContainerID = containerID
                      , stderrPayload = content
                      }
            Nothing -> NoBehavior
          Nothing -> NoBehavior
        )
behavior _ st (RegisterCmd clientID cmdID cmdstatus) = case cmdstatus of
  NotLaunched -> return (registerFailed cmdID st, NoBehavior)
  Launched -> do
    let (st', containerID) = registerLaunched cmdID st
    return
      ( st'
      , Rep clientID (URep.RepStart (URep.Start containerID cmdID))
      )
behavior _ st (DownstreamEvent msg) = case msg of
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
    return
      ( registerAwaiting cmdID
          (mkCmd spec (if detachCmd then Nothing else Just clientid))
          runContainerID .
          createContainer runContainerID $
          st
      , StartChild clientid cmdID (cmd spec) (args spec) (env spec)
      )
  UReq.ReqKillContainer UReq.KillContainer {..} -> do
    let st' = removeContainer killContainerID st
    let cmds = getCmds st killContainerID
    return (st', KillChildren cmds)
  UReq.ReqSetPower _ -> return (st, NoBehavior)
  UReq.ReqKillCmd UReq.KillCmd {..} -> return (st, NoBehavior)
behavior _ st DoSensor = return (st, NoBehavior)
behavior _ st DoControl = return (st, NoBehavior)
behavior _ st DoShutdown = return (st, NoBehavior)
behavior _ st DoChildren = return (st, NoBehavior)

-- | The sensitive tag that has to be pattern-matched on the python side.
instance MessagePack Behavior where

  toObject NoBehavior = toObject ("noop" :: Text)
  toObject (Rep clientid msg) = toObject ("reply" :: Text, clientid, M.encodeT msg)
  toObject (Pub msg) = toObject ("publish" :: Text, M.encodeT msg)
  toObject (StartChild clientID cmdID cmd args env) = toObject ("cmd" :: Text, clientID, cmdID, cmd, args, env)
  toObject (KillChildren cmdIDs) = toObject ("kill" :: Text, cmdIDs)

  fromObject x = to <$> gFromObject x
