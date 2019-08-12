{-|
Module      : Nrm.Behavior
Copyright   : (c) UChicago Argonne, 2019
License     : BSD3
Maintainer  : fre@freux.fr
-}
module Nrm.Behavior
  ( behavior
  , Behavior (..)
  , SendAPI (..)
  , RecvAPI (..)
  , NrmEvent (..)
  )
where

{-import Nrm.Types.Configuration-}
import Data.MessagePack
import Nrm.Types.Application
import Nrm.Types.NrmState
import Protolude

data SendAPI = UpstreamPub | UpstreamRep
  deriving (Generic)

deriving instance MessagePack SendAPI

data RecvAPI = DownstreamEvent | UpstreamReq
  deriving (Generic)

deriving instance MessagePack RecvAPI

data Behavior = NoBehavior | Send SendAPI ByteString | StartChild Command Arguments
  deriving (Generic)

deriving instance MessagePack Behavior

data NrmEvent = Recv RecvAPI ByteString | DoSensor | DoControl | DoShutdown | DoChildren
  deriving (Generic)

deriving instance MessagePack NrmEvent

behavior :: NrmEvent -> NrmState -> IO (NrmState, Behavior)
behavior (Recv DownstreamEvent _msg) st = do
  return (st, NoBehavior)
behavior (Recv UpstreamReq _msg) st = return (st, NoBehavior)
behavior DoSensor st = return (st, NoBehavior)
behavior DoControl st = return (st, NoBehavior)
behavior DoShutdown st = return (st, NoBehavior)
behavior DoChildren st = return (st, NoBehavior)
