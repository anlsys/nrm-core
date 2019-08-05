{-|
Module      : Nrm.Types.Messaging.UpstreamRep
Description : Nrm upstream messaging
Copyright   : (c) UChicago Argonne, 2019
License     : BSD3
Maintainer  : fre@freux.fr
-}
module Nrm.Types.Messaging.UpstreamRep
  ( Rep (..)
  )
where

import Data.Aeson
import Data.JSON.Schema
import Generics.Generic.Aeson
import Protolude hiding (Rep)

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
