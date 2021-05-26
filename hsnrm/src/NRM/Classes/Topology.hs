{-# LANGUAGE ScopedTypeVariables #-}

-- |
-- Module      : NRM.Classes.Topology
-- Copyright   : (c) UChicago Argonne, 2019
-- License     : BSD3
-- Maintainer  : fre@freux.fr
module NRM.Classes.Topology
  ( ToHwlocType (..),
  )
where

import Protolude

-- | translating to hwloc XML "type" field.
class ToHwlocType a where
  getType :: Text
