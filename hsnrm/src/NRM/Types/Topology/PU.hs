{-# LANGUAGE DerivingVia #-}

-- |
-- Module      : NRM.Types.Topology.PU
-- Copyright   : (c) UChicago Argonne, 2019
-- License     : BSD3
-- Maintainer  : swann@anl.gov
module NRM.Types.Topology.PU
  ( PU (..),
  )
where

import Data.Aeson
import Data.Data
import Data.MessagePack
import Protolude

-- | Record containing all information about a processing unit.
data PU = PU
  deriving (Show, Generic, Data, MessagePack, ToJSON, FromJSON)
