{-# LANGUAGE DerivingVia #-}

{-|
Module      : NRM.Types.DownstreamThreadID
Copyright   : (c) UChicago Argonne, 2019
License     : BSD3
Maintainer  : fre@freux.fr
-}
module NRM.Types.DownstreamThreadID
  ( DownstreamThreadID (..)
  , TaskID (..)
  , ThreadID (..)
  )
where

import Data.Aeson
import Data.Data
import Data.JSON.Schema
import Data.MessagePack
import NRM.Classes.Messaging
import NRM.Types.Process as P
import NRM.Types.CmdID
import Protolude

data DownstreamThreadID
  = DownstreamThreadID
      { cmdID :: CmdID
      , processID :: P.ProcessID
      , taskID :: TaskID
      , threadID :: ThreadID
      }
  deriving (Eq, Ord, Show, Generic, Data, MessagePack, ToJSONKey, FromJSONKey)
  deriving (JSONSchema, ToJSON, FromJSON) via GenericJSON DownstreamThreadID

newtype TaskID = TaskID {fromTaskID :: Text}
  deriving (Eq, Ord, Show, Read, Generic, Data, MessagePack)
  deriving (JSONSchema, ToJSON, FromJSON) via GenericJSON TaskID

newtype ThreadID = ThreadID Int
  deriving (Eq, Ord, Show, Read, Generic, Data, MessagePack)
  deriving (JSONSchema, ToJSON, FromJSON) via GenericJSON ThreadID
  deriving (Num, Real, Enum, Integral) via Int
