{-|
Module      : Nrm.Messaging.Upstream
Description : Nrm upstream messaging
Copyright   : (c) UChicago Argonne, 2019
License     : BSD3
Maintainer  : fre@freux.fr
-}
module Nrm.Messaging.Upstream
  ( Event (..)
  )
where

import Data.Aeson
import Data.JSON.Schema
import Generics.Generic.Aeson
import Protolude

data Pub
  = Power
      { total :: Double
      , limit :: Double
      }
  | ContainerStart
      { container_uuid :: Text
      , errno :: Int
      , power :: Text
      }
  | ContainerExit
      { container_uuid :: Text
      , profile_data :: Map Text Text
      }
  | Performance
      { container_uuid :: Text
      , payload :: Int
      }
  | Progress
      { application_uuid :: Text
      , payload :: Int
      }
  | Control
      { powercap :: Int
      , energy :: Double
      , performance :: Double
      , control_time :: Double
      , feedback_time :: Double
      }
  deriving (Generic)

instance ToJSON Pub where

  toJSON = gtoJson

instance FromJSON Pub where

  parseJSON = gparseJson

instance JSONSchema Pub where

  schema = gSchema

data Rep
  = List
      { containers :: [Text]
      }
  | Stdout
      { container_uuid :: Text
      , payload :: Text
      }
  | Stderr
      { container_uuid :: Text
      , payload :: Text
      }
  | Start
      { container_uuid :: Text
      , pid :: Int
      }
  | ProcessExit
      { container_uuid :: Text
      , status :: Text
      }
  | GetPower
      { limit :: Text
      }
  deriving (Generic)

instance ToJSON Rep where

  toJSON = gtoJson

instance FromJSON Rep where

  parseJSON = gparseJson

instance JSONSchema Rep where

  schema = gSchema

data Req
  = List
  | Run
      { manifest :: Text
      , path :: Text
      , args :: [Text]
      , runcontainer_uuid :: Text
      , environ :: [(Text, Text)]
      }
  | Kill
      { container_uuid :: Text
      }
  | Setpower
      { limit :: Text
      }
  deriving (Generic)

instance ToJSON Req where

  toJSON = gtoJson

instance FromJSON Req where

  parseJSON = gparseJson

instance JSONSchema Req where

  schema = gSchema
