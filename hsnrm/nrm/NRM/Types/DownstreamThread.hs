{-# LANGUAGE DerivingVia #-}

{-|
Module      : NRM.Types.Downstream
Copyright   : (c) UChicago Argonne, 2019
License     : BSD3
Maintainer  : fre@freux.fr
-}
module NRM.Types.DownstreamThread
  ( DownstreamThreadID (..)
  , DownstreamThread (..)
  )
where

import Data.Aeson
import Data.JSON.Schema
import Data.MessagePack
import NRM.Classes.Messaging
import NRM.Types.Process as P
import Protolude

data DownstreamThreadID
  = DownstreamThreadID
      { libnrmcmdID :: P.ProcessID
      , libnrmprocessID :: P.ProcessID
      , libnrmtaskID :: TaskID
      , libnrmthreadID :: ThreadID
      }
  deriving (Eq, Ord, Show, Generic, MessagePack, ToJSONKey, FromJSONKey)
  deriving (JSONSchema, ToJSON, FromJSON) via GenericJSON DownstreamThreadID

data DownstreamThread = DownstreamThread
  deriving (Eq, Ord, Show, Generic, MessagePack)
  deriving (JSONSchema, ToJSON, FromJSON) via GenericJSON DownstreamThread

{-instance JSONSchema DownstreamThreadID where-}

{-schema Proxy = schema (Proxy :: Proxy Text)-}
newtype TaskID = TaskID Int
  deriving (Eq, Ord, Show, Read, Generic, MessagePack)
  deriving (JSONSchema, ToJSON, FromJSON) via GenericJSON TaskID

newtype ThreadID = ThreadID Int
  deriving (Eq, Ord, Show, Read, Generic, MessagePack)
  deriving (JSONSchema, ToJSON, FromJSON) via GenericJSON ThreadID
