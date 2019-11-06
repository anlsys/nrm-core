{-# LANGUAGE DerivingVia #-}

-- |
-- Module      : NRM.Types.Topology.PackageID
-- Copyright   : (c) UChicago Argonne, 2019
-- License     : BSD3
-- Maintainer  : fre@freux.fr
module NRM.Types.Topology.PackageID
  ( PackageID (..),
  )
where

import Data.Aeson
import Data.Data
import Data.MessagePack
import NRM.Classes.Messaging
import NRM.Classes.Topology
import Protolude
import Refined.Orphan.Aeson ()

-- | A Package OS identifier.
newtype PackageID = PackageID Int
  deriving (Eq, Ord, Show, Read, Generic, Data, FromJSONKey, ToJSONKey, MessagePack)
  deriving (FromJSON, ToJSON, JSONSchema) via GenericJSON PackageID

instance IdFromString PackageID where
  idFromString s = PackageID <$> readMaybe s

instance ToHwlocType PackageID where
  getType _ = "Package"
