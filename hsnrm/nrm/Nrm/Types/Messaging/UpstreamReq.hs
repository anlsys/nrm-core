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
  )
where

import Data.Aeson
import Data.JSON.Schema
import Generics.Generic.Aeson
import Nrm.Classes.Messaging
import qualified Nrm.Types.Process as P
import qualified Nrm.Types.Container as C
import Nrm.Types.Manifest
import qualified Nrm.Types.Units as U
import Protolude

data Req
  = ReqContainerList ContainerList
  | ReqRun Run
  | ReqKill Kill
  | ReqSetPower SetPower
  deriving (Show, Generic)

data Run
  = Run
      { manifest :: Manifest
      , path :: P.Command
      , args :: P.Arguments
      , runContainerID :: C.ContainerID
      , environ :: [(Text, Text)]
      }
  deriving (Show, Generic)

newtype Kill
  = Kill
      { killContainerID :: C.ContainerID
      }
  deriving (Show, Generic)

newtype SetPower
  = SetPower
      { limit :: U.Power
      }
  deriving (Show, Generic)

data ContainerList = ContainerList
  deriving (Show, Generic)

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
