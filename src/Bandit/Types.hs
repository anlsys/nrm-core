-- |
-- Module      : Bandit.Util
-- Copyright   : (c) 2019, UChicago Argonne, LLC.
-- License     : MIT
-- Maintainer  : fre@freux.fr
--
-- Utility functions for MAB algorithms.
module Bandit.Types
  ( ZeroOne,
    Bandit.Types.zero,
    Bandit.Types.one,
  )
where

import Protolude
import Refined
import Refined.Unsafe

-- | Type alias for a \(\mathbb{L}=[0,1]\) refined value.
type ZeroOne l = (Refined (FromTo 0 1) l)

-- | 0
zero :: (Ord a, Num a) => ZeroOne a
zero = unsafeRefine 0

-- | 1
one :: (Ord a, Num a) => ZeroOne a
one = unsafeRefine 1
