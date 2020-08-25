{-# LANGUAGE DerivingVia #-}

-- |
-- Module      : NRM.Types.DownstreamThreadID
-- Copyright   : (c) UChicago Argonne, 2019
-- License     : BSD3
-- Maintainer  : fre@freux.fr
module NRM.Types.DownstreamThreadID
  ( DownstreamThreadID (..),
    TaskID (..),
    ThreadID (..),
  )
where

import Data.Aeson
import Data.JSON.Schema
import Data.MessagePack
import NRM.Classes.Messaging
import NRM.Types.CmdID
import NRM.Types.Process as P
import Protolude

data DownstreamThreadID
  = DownstreamThreadID
      { cmdID :: CmdID,
        processID :: P.ProcessID,
        taskID :: TaskID,
        threadID :: ThreadID,
        rankID :: RankID
      }
  deriving
    ( Eq,
      Ord,
      Show,
      Generic,
      MessagePack,
      ToJSONKey,
      FromJSONKey
    )
  deriving
    ( JSONSchema,
      ToJSON,
      IsString,
      FromJSON
    )
    via GenericJSON DownstreamThreadID


newtype RankID = RankID {fromRankID :: Int}
  deriving (Eq, Ord, Show, Read, Generic, MessagePack)
  deriving
    ( JSONSchema,
      ToJSON,
      FromJSON,
      Num,
      Real,
      Enum,
      Integral
    )
    via Int

newtype TaskID = TaskID {fromTaskID :: Text}
  deriving (Eq, Ord, Show, Read, Generic, MessagePack)
  deriving (JSONSchema, ToJSON, FromJSON) via Text

newtype ThreadID = ThreadID Int
  deriving (Eq, Ord, Show, Read, Generic, MessagePack)
  deriving
    ( JSONSchema,
      ToJSON,
      FromJSON,
      Num,
      Real,
      Enum,
      Integral
    )
    via Int
