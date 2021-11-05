{-# LANGUAGE DerivingVia #-}

-- |
-- Module      : NRM.Types.Topology.Core
-- Copyright   : (c) UChicago Argonne, 2019
-- License     : BSD3
-- Maintainer  : swann@anl.gov
module NRM.Types.Topology.Core
  ( Core (..),
  )
where

import Data.Aeson
import Data.Data
import Data.MessagePack
import Protolude

-- | Record containing all information about a CPU Core.
data Core = Core
  deriving (Show, Generic, Data, MessagePack, ToJSON, FromJSON)
