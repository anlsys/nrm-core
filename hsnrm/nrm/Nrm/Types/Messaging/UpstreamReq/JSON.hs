{-|
Module      : Nrm.Types.Messaging.UpstreamReq.JSON
Copyright   : (c) UChicago Argonne, 2019
License     : BSD3
Maintainer  : fre@freux.fr
-}
module Nrm.Types.Messaging.UpstreamReq.JSON
  ( Req (..)
  )
where

import Data.Aeson
import Data.JSON.Schema
import Generics.Generic.Aeson
import Nrm.Types.Container
import Nrm.Types.Manifest
import Protolude

data Req
  = ContainerList
  | Run
      { manifest :: Manifest
      , path :: Text
      , args :: [Text]
      , container_uuid :: Text
      , environ :: [(Text, Text)]
      }
  | Kill
      { container_uuid :: Text
      }
  | SetPower
      { limit :: Text
      }
  deriving (Show, Generic)
