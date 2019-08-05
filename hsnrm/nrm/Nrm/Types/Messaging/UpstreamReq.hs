{-|
Module      : Nrm.Types.Messaging.UpstreamReq
Description : Nrm upstream messaging
Copyright   : (c) UChicago Argonne, 2019
License     : BSD3
Maintainer  : fre@freux.fr
-}
module Nrm.Types.Messaging.UpstreamReq
  ( Req (..)
  )
where

import Data.Aeson
import Data.JSON.Schema
import Generics.Generic.Aeson
import Protolude

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
