{-# LANGUAGE DerivingVia #-}

{-|
Module      : NRM.Types.DownstreamCmdClient
Copyright   : (c) UChicago Argonne, 2019
License     : BSD3
Maintainer  : fre@freux.fr
-}
module NRM.Types.DownstreamCmdClient
  ( DownstreamCmdID (..)
  , DownstreamCmd (..)
  )
where

import Data.Aeson
import Data.JSON.Schema
import Data.MessagePack
import Data.String (IsString (..))
import NRM.Classes.Messaging
import NRM.Types.Process as P
import Protolude

newtype DownstreamCmdID
  = DownstreamCmdID
      { perfcmdID :: P.ProcessID
      }
  deriving (Eq, Ord, Show, Generic, MessagePack, ToJSONKey, FromJSONKey)
  deriving (JSONSchema, ToJSON, FromJSON) via GenericJSON DownstreamCmdID

data DownstreamCmd = DownstreamCmd
  deriving (Eq, Ord, Show, Generic, MessagePack)
  deriving (JSONSchema, ToJSON, FromJSON) via GenericJSON DownstreamCmd

instance IsString DownstreamCmdID where

  fromString x = fromMaybe (panic "couldn't decode DownstreamCmdID") (Data.Aeson.decode $ toS x)
