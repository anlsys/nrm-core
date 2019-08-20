{-|
Module      : Nrm.Behavior
Copyright   : (c) UChicago Argonne, 2019
License     : BSD3
Maintainer  : fre@freux.fr
-}
module Nrm.Behavior
  ( behavior
  , Behavior (..)
  , NrmEvent (..)
  )
where

import qualified Data.Map as DM
import Data.MessagePack
import qualified Nrm.Classes.Messaging as M
import qualified Nrm.Types.Configuration as Cfg
import qualified Nrm.Types.Container as C
import Nrm.Types.Messaging.DownstreamEvent as DEvent
import Nrm.Types.Messaging.UpstreamPub as UPub
import Nrm.Types.Messaging.UpstreamRep as URep
import Nrm.Types.Messaging.UpstreamReq as UReq
import qualified Nrm.Types.NrmState as S
import Nrm.Types.Process
import qualified Nrm.Types.UpstreamClient as UC
import Protolude

data Behavior = NoBehavior | Rep UC.UpstreamClientID URep.Rep | Pub UPub.Pub | StartChild CmdID Command Arguments Env
  deriving (Generic)

data NrmEvent = Req UC.UpstreamClientID UReq.Req | Event DEvent.Event | DoSensor | DoControl | DoShutdown | DoChildren
  deriving (Generic, MessagePack)

behavior :: Cfg.Cfg -> S.NrmState -> NrmEvent -> IO (S.NrmState, Behavior)
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
      rep = URep.ContainerList (DM.toList (S.containers st))
  UReq.ReqGetState _ ->
    return (st, Rep clientid (URep.RepGetState (URep.GetState st)))
  UReq.ReqGetConfig _ ->
    return (st, Rep clientid (URep.RepGetConfig (URep.GetConfig c)))
  UReq.ReqRun Run {..} -> do
    cmdID <- nextCmdID <&> fromMaybe (panic "couldn't generate next cmd id")
    (containerID, container) <-
      case DM.lookup runContainerID (S.containers st) of
        Nothing -> do
          containerID <-
            C.nextContainerID <&>
              fromMaybe (panic "couldn't generate next container id")
          return
            ( containerID
            , C.Container {awaiting = [cmdID], cmds = []}
            )
        Just container ->
          return
            ( runContainerID
            , container {C.awaiting = C.awaiting container ++ [cmdID]}
            )
    let containers' = DM.insert containerID container (S.containers st)
    return (st {S.containers = containers'}, StartChild cmdID path args environ)
  UReq.ReqKill _ -> return (st, NoBehavior)
  UReq.ReqSetPower _ -> return (st, NoBehavior)
behavior _ st DoSensor = return (st, NoBehavior)
behavior _ st DoControl = return (st, NoBehavior)
behavior _ st DoShutdown = return (st, NoBehavior)
behavior _ st DoChildren = return (st, NoBehavior)

-- | The sensitive code that has to be pattern-matched on the python side.
instance MessagePack Behavior where

  toObject NoBehavior = toObject ("noop" :: Text)
  toObject (Rep clientid msg) = toObject ("reply" :: Text, clientid, M.encodeT msg)
  toObject (Pub msg) = toObject ("publish" :: Text, msg)
  toObject (StartChild cmdID cmd args env) = toObject ("cmd" :: Text, cmdID, cmd, args, env)

  fromObject x = to <$> gFromObject x
