{-# LANGUAGE DerivingVia #-}
{-# OPTIONS_GHC -fno-warn-orphans #-}

-- |
-- Module      : NRM.Orphans.ZeroOne
-- Copyright   : (c) UChicago Argonne, 2019
-- License     : BSD3
-- Maintainer  : swann@anl.gov
--
-- Instances for ZeroOne and Intervals of such.
module NRM.Orphans.ZeroOne
  (
  )
where

import Bandit.Types
import Data.Aeson
import Data.JSON.Schema
import Data.MessagePack
import Dhall
import NRM.Classes.Messaging
import NRM.Orphans.Refined ()
import Numeric.Interval
import Protolude
import Refined.Orphan.Aeson ()

deriving instance (MessagePack a, Ord a, Num a) => MessagePack (Interval (ZeroOne a))

instance (FromDhall a, Ord a, Num a) => FromDhall (Interval (ZeroOne a))

instance (ToDhall a) => ToDhall (Interval (ZeroOne a))

deriving via GenericJSON (Interval (ZeroOne a)) instance (JSONSchema a) => JSONSchema (Interval (ZeroOne a))

deriving via GenericJSON (Interval (ZeroOne a)) instance (ToJSON a, Ord a, Num a) => ToJSON (Interval (ZeroOne a))

deriving via GenericJSON (Interval (ZeroOne a)) instance (FromJSON a, Ord a, Num a) => FromJSON (Interval (ZeroOne a))
