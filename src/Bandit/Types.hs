-- |
-- Module      : Bandit.Util
-- Copyright   : (c) 2019, UChicago Argonne, LLC.
-- License     : MIT
-- Maintainer  : fre@freux.fr
--
-- Utility functions for MAB algorithms.
module Bandit.Types
  ( Arms (..),
    ObliviousRep (..),
    FixedRate (..),
    InverseSqrtRate (..),
    AlphaUCBInvLFPhi (..),
    ZeroOne,
    Bandit.Types.zero,
    Bandit.Types.one,
    rewardCostBijection,
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

rewardCostBijection :: (Ord a, Num a) => ZeroOne a -> ZeroOne a
rewardCostBijection x = unsafeRefine (1 - unrefine x)

-- | Arms a represents a set of possible actions.
newtype Arms a = Arms (Protolude.NonEmpty a)
  deriving (Show, Generic)

-- | Oblivious Categorical Expert Representation
newtype ObliviousRep a
  = ObliviousRep (Protolude.NonEmpty (ZeroOne Double, a))
  deriving (Show, Generic)

newtype FixedRate = FixedRate Double
  deriving (Show, Generic)

newtype InverseSqrtRate = InverseSqrtRate Double
  deriving (Show, Generic)

data AlphaUCBInvLFPhi = AlphaUCBInvLFPhi
  deriving (Show, Generic)
