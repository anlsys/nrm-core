{-|
Module      : Nrm.Types.DownstreamClient
Copyright   : (c) UChicago Argonne, 2019
License     : BSD3
Maintainer  : fre@freux.fr
-}
module Nrm.Types.DownstreamClient
  ( DownstreamPerfID (..)
  , DownstreamLibnrmID (..)
  )
where

import qualified Data.Aeson as A
import Data.Aeson
import Data.JSON.Schema
import Data.MessagePack
import Generics.Generic.Aeson
import Nrm.Types.Process as P
import Protolude

data DownstreamPerfID
  = DownstreamPerfID
      { perfcmdID :: P.ProcessID
      }
  deriving (Eq, Ord, Show, Generic)

data DownstreamLibnrmID
  = DownstreamLibnrmID
      { libnrmcmdID :: P.ProcessID
      , libnrmprocessID :: P.ProcessID
      , libnrmtaskID :: P.TaskID
      , libnrmthreadID :: P.ThreadID
      }
  deriving (Eq, Ord, Show, Generic)

instance ToJSON DownstreamPerfID where

  toJSON = gtoJson

instance FromJSON DownstreamPerfID where

  parseJSON = gparseJson

instance JSONSchema DownstreamPerfID where

  schema Proxy = schema (Proxy :: Proxy Text)

deriving instance MessagePack DownstreamPerfID

instance ToJSON DownstreamLibnrmID where

  toJSON = gtoJson

instance FromJSON DownstreamLibnrmID where

  parseJSON = gparseJson

instance JSONSchema DownstreamLibnrmID where

  schema Proxy = schema (Proxy :: Proxy Text)

deriving instance MessagePack DownstreamLibnrmID
