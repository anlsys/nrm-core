{-|
Module      : Nrm.Types.Messaging.UpstreamRep
Copyright   : (c) UChicago Argonne, 2019
License     : BSD3
Maintainer  : fre@freux.fr
-}
module Nrm.Types.Messaging.UpstreamRep
  ( Rep (..)
  , ContainerList (..)
  , Stdout (..)
  , Stderr (..)
  , Start (..)
  , ProcessExit (..)
  , GetPower (..)
  )
where

import Data.Aeson
import Data.JSON.Schema
import Generics.Generic.Aeson
import Protolude hiding (Rep)

data Rep
  = RepList ContainerList
  | RepStdout Stdout
  | RepStderr Stderr
  | RepStart Start
  | RepProcessExit ProcessExit
  | RepGetPower GetPower
  deriving (Show, Generic)

newtype ContainerList
  = ContainerList
      { containers :: [Text]
      }
  deriving (Show, Generic)

data Stdout
  = Stdout
      { stdoutContainerUUUID :: Text
      , stdoutPayload :: Text
      }
  deriving (Show, Generic)

data Stderr
  = Stderr
      { stderrContainerUUID :: Text
      , stderrPayload :: Text
      }
  deriving (Show, Generic)

data Start
  = Start
      { startContainerUUID :: Text
      , pid :: Int
      }
  deriving (Show, Generic)

data ProcessExit
  = ProcessExit
      { container_uuid :: Text
      , status :: Text
      }
  deriving (Show, Generic)

newtype GetPower
  = GetPower
      { limit :: Text
      }
  deriving (Show, Generic)

instance ToJSON ContainerList where

  toJSON = gtoJson

instance FromJSON ContainerList where

  parseJSON = gparseJson

instance JSONSchema ContainerList where

  schema = gSchema

instance ToJSON Stdout where

  toJSON = gtoJson

instance FromJSON Stdout where

  parseJSON = gparseJson

instance JSONSchema Stdout where

  schema = gSchema

instance ToJSON Stderr where

  toJSON = gtoJson

instance FromJSON Stderr where

  parseJSON = gparseJson

instance JSONSchema Stderr where

  schema = gSchema

instance ToJSON Start where

  toJSON = gtoJson

instance FromJSON Start where

  parseJSON = gparseJson

instance JSONSchema Start where

  schema = gSchema

instance ToJSON ProcessExit where

  toJSON = gtoJson

instance FromJSON ProcessExit where

  parseJSON = gparseJson

instance JSONSchema ProcessExit where

  schema = gSchema

instance ToJSON GetPower where

  toJSON = gtoJson

instance FromJSON GetPower where

  parseJSON = gparseJson

instance JSONSchema GetPower where

  schema = gSchema

instance ToJSON Rep where

  toJSON = gtoJson

instance FromJSON Rep where

  parseJSON = gparseJson

instance JSONSchema Rep where

  schema = gSchema
