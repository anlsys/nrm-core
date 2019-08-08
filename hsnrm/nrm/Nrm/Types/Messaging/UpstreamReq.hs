{-|
Module      : Nrm.Types.Messaging.UpstreamReq
Copyright   : (c) UChicago Argonne, 2019
License     : BSD3
Maintainer  : fre@freux.fr
-}
module Nrm.Types.Messaging.UpstreamReq
  ( Req (..)
  , RunRequest (..)
  , ListRequest (..)
  , KillRequest (..)
  , SetPowerRequest (..)
  )
where

import Data.Aeson
import Data.JSON.Schema
import Generics.Generic.Aeson
import Nrm.Types.Container
import Nrm.Types.Manifest
import Protolude

data RunRequest
  = RunRequest
      { manifest :: Manifest
      , path :: Text
      , args :: [Text]
      , runcontainer_uuid :: ContainerUUID
      , environ :: [(Text, Text)]
      }
  deriving (Show, Generic)

newtype KillRequest
  = KillRequest
      { container_uuid :: Text
      }
  deriving (Show, Generic)

newtype SetPowerRequest
  = SetPowerRequest
      { limit :: Text
      }
  deriving (Show, Generic)

data ListRequest = ListRequest
  deriving (Show, Generic)

data Req
  = List ListRequest
  | Run RunRequest
  | Kill KillRequest
  | SetPower SetPowerRequest
  deriving (Show, Generic)

instance ToJSON ListRequest where

  toJSON = gtoJson

instance FromJSON ListRequest where

  parseJSON = gparseJson

instance JSONSchema ListRequest where

  schema = gSchema

instance ToJSON RunRequest where

  toJSON = gtoJson

instance FromJSON RunRequest where

  parseJSON = gparseJson

instance JSONSchema RunRequest where

  schema = gSchema

instance ToJSON SetPowerRequest where

  toJSON = gtoJson

instance FromJSON SetPowerRequest where

  parseJSON = gparseJson

instance JSONSchema SetPowerRequest where

  schema = gSchema

instance ToJSON KillRequest where

  toJSON = gtoJson

instance FromJSON KillRequest where

  parseJSON = gparseJson

instance JSONSchema KillRequest where

  schema = gSchema

instance ToJSON Req where

  toJSON = gtoJson

instance FromJSON Req where

  parseJSON = gparseJson

instance JSONSchema Req where

  schema = gSchema
