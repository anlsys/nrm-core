-- |
-- Module      : Bandit.Util
-- Copyright   : (c) 2019, UChicago Argonne, LLC.
-- License     : BSD3
-- Maintainer  : swann@anl.gov
--
-- Utility functions for MAB algorithms.
module Bandit.Types
  ( Arms (..),
    ObliviousRep (..),
    FixedRate (..),
    InverseSqrtRate (..),
    ZeroOne,
    Bandit.Types.zero,
    Bandit.Types.one,
  )
where

import Protolude
import Refined
import Refined.Unsafe

-- | Type alias for a \([0,1]\) refined value.
type ZeroOne l = (Refined (FromTo 0 1) l)

-- | 0
zero :: (Ord a, Num a) => ZeroOne a
zero = unsafeRefine 0

-- | 1
one :: (Ord a, Num a) => ZeroOne a
one = unsafeRefine 1

-- | Arms a represents a set of possible actions.
newtype Arms a = Arms (Protolude.NonEmpty a)
  deriving (Show, Generic)

-- | Oblivious Categorical Expert Representation
newtype ObliviousRep a
  = ObliviousRep (Protolude.NonEmpty (ZeroOne Double, a))
  deriving (Generic)

newtype FixedRate = FixedRate Double
  deriving (Generic)

newtype InverseSqrtRate = InverseSqrtRate Double
  deriving (Generic)
