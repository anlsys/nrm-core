{-|
Module      : Nrm.Types.Messaging.UpstreamPub
Copyright   : (c) UChicago Argonne, 2019
License     : BSD3
Maintainer  : fre@freux.fr
-}
module Nrm.Types.Messaging.UpstreamPub
  ( Pub (..)
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
