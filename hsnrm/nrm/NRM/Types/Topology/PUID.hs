{-# LANGUAGE DerivingVia #-}

-- |
-- Module      : NRM.Types.Topology.PUID
-- Copyright   : (c) UChicago Argonne, 2019
-- License     : BSD3
-- Maintainer  : fre@freux.fr
module NRM.Types.Topology.PUID
  ( PUID (..),
  )
where

import Data.Aeson
import Data.Data
import Data.MessagePack
import NRM.Classes.Messaging
import NRM.Classes.Topology
import Protolude

-- | A Processing Unit OS identifier.
newtype PUID = PUID Int
  deriving (Eq, Ord, Show, Generic, Data, ToJSONKey, FromJSONKey, MessagePack)
  deriving (JSONSchema, ToJSON, FromJSON) via GenericJSON PUID

instance ToHwlocType PUID where
  getType _ = "PU"
