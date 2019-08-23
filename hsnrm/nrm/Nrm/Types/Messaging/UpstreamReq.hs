{-|
Module      : Nrm.Types.Messaging.UpstreamReq
Copyright   : (c) UChicago Argonne, 2019
License     : BSD3
Maintainer  : fre@freux.fr
-}
module Nrm.Types.Messaging.UpstreamReq
  ( Req (..)
  , Run (..)
  , ContainerList (..)
  , Kill (..)
  , SetPower (..)
  , GetConfig(..)
  , GetState(..)
  )
where

import Data.Aeson
import Data.JSON.Schema
import Data.MessagePack
import Generics.Generic.Aeson
import Nrm.Classes.Messaging
import qualified Nrm.Types.Container as C
import Nrm.Types.Manifest
import qualified Nrm.Types.Process as P
import qualified Nrm.Types.Units as U
import Protolude

data Req
  = ReqContainerList ContainerList
  | ReqRun Run
  | ReqKill Kill
  | ReqSetPower SetPower
  | ReqGetState GetState
  | ReqGetConfig GetConfig
  deriving (Show, Generic, MessagePack)

data Run
  = Run
      { manifest :: Manifest
      , spec :: P.CmdSpec
      , runContainerID :: C.ContainerID
      }
  deriving (Show, Generic, MessagePack)

newtype Kill
  = Kill
      { killContainerID :: C.ContainerID
      }
  deriving (Show, Generic, MessagePack)

newtype SetPower
  = SetPower
      { limit :: U.Power
      }
  deriving (Show, Generic, MessagePack)

data ContainerList = ContainerList
  deriving (Show, Generic, MessagePack)

data GetState = GetState
  deriving (Show, Generic, MessagePack)

data GetConfig = GetConfig
  deriving (Show, Generic, MessagePack)

instance ToJSON Kill where

  toJSON = gtoJson

instance FromJSON Kill where

  parseJSON = gparseJson

instance JSONSchema Kill where

  schema = gSchema

instance ToJSON SetPower where

  toJSON = gtoJson

instance FromJSON SetPower where

  parseJSON = gparseJson

instance JSONSchema SetPower where

  schema = gSchema

instance ToJSON ContainerList where

  toJSON = gtoJson

instance FromJSON ContainerList where

  parseJSON = gparseJson

instance JSONSchema ContainerList where

  schema = gSchema

instance NrmMessage Req Req where

  toJ = identity

  fromJ = identity

instance ToJSON Run where

  toJSON = gtoJson

instance FromJSON Run where

  parseJSON = gparseJson

instance JSONSchema Run where

  schema = gSchema

instance ToJSON GetState where

  toJSON = gtoJson

instance FromJSON GetState where

  parseJSON = gparseJson

instance JSONSchema GetState where

  schema = gSchema


instance ToJSON GetConfig where

  toJSON = gtoJson

instance FromJSON GetConfig where

  parseJSON = gparseJson

instance JSONSchema GetConfig where

  schema = gSchema

