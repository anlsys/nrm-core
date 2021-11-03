{-# LANGUAGE DerivingVia #-}

-- |
-- Module      : NRM.Types.Topology.CoreID
-- Copyright   : (c) UChicago Argonne, 2019
-- License     : BSD3
-- Maintainer  : swann@anl.gov
module NRM.Types.Topology.CoreID
  ( CoreID (..),
  )
where

import Data.Aeson
import Data.Data
import Data.JSON.Schema
import Data.MessagePack
import NRM.Classes.Messaging
import NRM.Classes.Topology
import Protolude

-- | A CPU Core OS identifier.
newtype CoreID = CoreID Int
  deriving (Show, Eq, Ord, Generic, Data, ToJSONKey, FromJSONKey, MessagePack)
  deriving (JSONSchema, FromJSON, ToJSON) via GenericJSON CoreID

instance ToHwlocType CoreID where
  getType = "Core"
