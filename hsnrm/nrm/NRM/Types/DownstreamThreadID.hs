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

newtype TaskID = TaskID Int
  deriving (Eq, Ord, Show, Read, Generic, MessagePack)
  deriving (JSONSchema, ToJSON, FromJSON) via GenericJSON TaskID
  deriving (Num, Real, Enum, Integral) via Int

newtype ThreadID = ThreadID Int
  deriving (Eq, Ord, Show, Read, Generic, MessagePack)
  deriving (JSONSchema, ToJSON, FromJSON) via GenericJSON ThreadID
  deriving (Num, Real, Enum, Integral) via Int
