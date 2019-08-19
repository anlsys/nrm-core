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

import Data.MessagePack
import qualified Nrm.Classes.Messaging as M
import Nrm.Types.Messaging.DownstreamEvent as DEvent
import Nrm.Types.Messaging.UpstreamPub as UPub
import Nrm.Types.Messaging.UpstreamRep as URep
import Nrm.Types.Messaging.UpstreamReq as UReq
import qualified Nrm.Types.NrmState as S
import Nrm.Types.Process
import qualified Nrm.Types.UpstreamClient as UC
import Protolude

data Behavior = NoBehavior | Rep UC.UpstreamClientID URep.Rep | Pub UPub.Pub | StartChild Command Arguments
  deriving (Generic)

instance MessagePack Behavior where

  toObject (NoBehavior) = toObject ("noop" :: Text)
  toObject (Rep clientid msg) = toObject ("reply" :: Text, clientid, M.encodeT msg)
  toObject (Pub msg) = toObject ("publish" :: Text, msg)
  toObject (StartChild cmd args) = toObject ("cmd" :: Text, cmd, args)

  fromObject x = to <$> gFromObject x

data NrmEvent = Req UC.UpstreamClientID UReq.Req | Event DEvent.Event | DoSensor | DoControl | DoShutdown | DoChildren
  deriving (Generic, MessagePack)

behavior :: S.NrmState -> NrmEvent -> IO (S.NrmState, Behavior)
behavior st (Event msg) = case msg of
  DEvent.ThreadStart _ -> return (st, NoBehavior)
  DEvent.ThreadProgress _ _ -> return (st, NoBehavior)
  DEvent.ThreadPhaseContext _ _ -> return (st, NoBehavior)
  DEvent.ThreadExit _ -> return (st, NoBehavior)
  DEvent.CmdStart _ -> return (st, NoBehavior)
  DEvent.CmdPerformance _ _ -> return (st, NoBehavior)
  DEvent.CmdExit _ -> return (st, NoBehavior)
behavior st (Req clientid msg) = case msg of
  UReq.ReqContainerList _ -> return (st, Rep clientid (URep.RepList lcontainers))
    where
      lcontainers = URep.ContainerList []
  UReq.ReqRun _ -> return (st, NoBehavior)
  UReq.ReqKill _ -> return (st, NoBehavior)
  UReq.ReqSetPower _ -> return (st, NoBehavior)
behavior st DoSensor = return (st, NoBehavior)
behavior st DoControl = return (st, NoBehavior)
behavior st DoShutdown = return (st, NoBehavior)
behavior st DoChildren = return (st, NoBehavior)
