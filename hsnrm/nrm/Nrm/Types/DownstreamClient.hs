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
import Data.String (IsString (..))
import Generics.Generic.Aeson
import Nrm.Types.Process as P
import Protolude

newtype DownstreamCmdID
  = DownstreamCmdID
      { perfcmdID :: P.ProcessID
      }
  deriving (Eq, Ord, Show, Generic, MessagePack, ToJSONKey, FromJSONKey)

data DownstreamCmd = DownstreamCmd
  deriving (Eq, Ord, Show, Generic, MessagePack, ToJSON, FromJSON)

instance JSONSchema DownstreamCmd where

  schema = gSchema

data DownstreamThreadID
  = DownstreamThreadID
      { libnrmcmdID :: P.ProcessID
      , libnrmprocessID :: P.ProcessID
      , libnrmtaskID :: P.TaskID
      , libnrmthreadID :: P.ThreadID
      }
  deriving (Eq, Ord, Show, Generic, MessagePack, ToJSONKey, FromJSONKey)

instance IsString DownstreamCmdID where

  fromString x = fromMaybe (panic "couldn't decode DownstreamCmdID") (decode $ toS x)

instance IsString DownstreamThreadID where

  fromString x = fromMaybe (panic "couldn't decode DownstreamCmdID") (decode $ toS x)

data DownstreamThread = DownstreamThread
  deriving (Eq, Ord, Show, Generic, MessagePack, ToJSON, FromJSON)

instance JSONSchema DownstreamThread where

  schema = gSchema

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
