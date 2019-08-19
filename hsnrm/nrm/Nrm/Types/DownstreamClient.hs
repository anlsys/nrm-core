{-|
Module      : Nrm.Types.DownstreamClient
Copyright   : (c) UChicago Argonne, 2019
License     : BSD3
Maintainer  : fre@freux.fr
-}
module Nrm.Types.DownstreamClient
  ( DownstreamCmdID (..)
  , DownstreamCmd (..)
  , DownstreamThreadID (..)
  , DownstreamThread (..)
  )
where

import qualified Data.Aeson as A
import Data.Aeson
import Data.JSON.Schema
import Data.MessagePack
import Generics.Generic.Aeson
import Nrm.Types.Process as P
import Protolude

data DownstreamCmdID
  = DownstreamCmdID
      { perfcmdID :: P.ProcessID
      }
  deriving (Eq, Ord, Show, Generic)

data DownstreamCmd = DownstreamCmd
  deriving (Eq, Ord, Show, Generic)

data DownstreamThreadID
  = DownstreamThreadID
      { libnrmcmdID :: P.ProcessID
      , libnrmprocessID :: P.ProcessID
      , libnrmtaskID :: P.TaskID
      , libnrmthreadID :: P.ThreadID
      }
  deriving (Eq, Ord, Show, Generic)

data DownstreamThread = DownstreamThread
  deriving (Eq, Ord, Show, Generic)

-- JSON Instances

instance ToJSON DownstreamCmdID where

  toJSON = gtoJson

instance FromJSON DownstreamCmdID where

  parseJSON = gparseJson

instance JSONSchema DownstreamCmdID where

  schema Proxy = schema (Proxy :: Proxy Text)


instance ToJSON DownstreamThreadID where

  toJSON = gtoJson

instance FromJSON DownstreamThreadID where

  parseJSON = gparseJson

instance JSONSchema DownstreamThreadID where

  schema Proxy = schema (Proxy :: Proxy Text)

 -- MessagePack Instances

deriving instance MessagePack DownstreamThreadID

deriving instance MessagePack DownstreamCmd

deriving instance MessagePack DownstreamThread
deriving instance MessagePack DownstreamCmdID
