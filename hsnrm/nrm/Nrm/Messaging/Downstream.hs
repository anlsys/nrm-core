{-|
Module      : Nrm.Messaging.Downstream
Description : Nrm downstream messaging
Copyright   : (c) UChicago Argonne, 2019
License     : BSD3
Maintainer  : fre@freux.fr
-}
module Nrm.Messaging.Downstream
  ( Event (..)
  )
where

import Data.Aeson
import Data.JSON.Schema
import Generics.Generic.Aeson
import Nrm.Codegen.CHeader
import Protolude

data Event
  = Start
      { container_uuid :: Text
      , application_uuid :: Text
      }
  | Exit
      { application_uuid :: Text
      }
  | Performance
      { container_uuid :: Text
      , application_uuid :: Text
      , payload :: Int
      }
  | Progress
      { application_uuid :: Text
      , payload :: Int
      }
  | PhaseContext
      { cpu :: Int
      , startcompute :: Int
      , endcompute :: Int
      , startbarrier :: Int
      , endbarrier :: Int
      }
  deriving (Generic, CHeaderGen)

instance ToJSON Event where

  toJSON = gtoJson

instance FromJSON Event where

  parseJSON = gparseJson

instance JSONSchema Event where

  schema = gSchema
