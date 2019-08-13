{-|
Module      : Nrm.Types.Messaging.UpstreamReq
Copyright   : (c) UChicago Argonne, 2019
License     : BSD3
Maintainer  : fre@freux.fr
-}
module Nrm.Types.Messaging.UpstreamReq
  ( Req (..)
  , RunRequest (..)
  , ContainerListRequest (..)
  , KillRequest (..)
  , SetPowerRequest (..)
  )
where

import Nrm.Types.Container
import Nrm.Types.Manifest
import Nrm.Classes.Messaging
import qualified Nrm.Types.Messaging.UpstreamReq.JSON as J
import Protolude

data Req
  = ContainerList ContainerListRequest
  | Run RunRequest
  | Kill KillRequest
  | SetPower SetPowerRequest
  deriving (Show, Generic)

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

data ContainerListRequest = ContainerListRequest
  deriving (Show, Generic)

instance JSONLayer Req J.Req where
