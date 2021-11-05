{-# LANGUAGE DerivingVia #-}

-- |
-- Module      : NRM.Types.DownstreamClient
-- Copyright   : (c) UChicago Argonne, 2019
-- License     : BSD3
-- Maintainer  : swann@anl.gov
module NRM.Types.DownstreamClient
  ( DownstreamClientID (..),
  )
where

import Data.Aeson
import Data.Data
import Data.JSON.Schema
import Data.MessagePack
import NRM.Classes.Messaging
import Protolude

newtype DownstreamClientID = DownstreamClientID {fromDownstreamClientID :: Text}
  deriving (Eq, Ord, Show, Read, Generic, Data, MessagePack)
  deriving (JSONSchema, ToJSON, FromJSON) via GenericJSON DownstreamClientID
  deriving (StringConv Text) via Text

instance StringConv DownstreamClientID Text where
  strConv _ (DownstreamClientID x) = toS x
