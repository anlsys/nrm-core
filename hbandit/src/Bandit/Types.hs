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

-- | Turns a loss into a reward and vice-versa.
rewardCostBijection :: (Ord a, Num a) => ZeroOne a -> ZeroOne a
rewardCostBijection x = unsafeRefine (1 - unrefine x)

-- | Arms a represents a set of possible actions.
newtype Arms a = Arms (Protolude.NonEmpty a)
  deriving (Show, Generic)

-- | Oblivious Categorical Expert Representation
newtype ObliviousRep a
  = ObliviousRep (Protolude.NonEmpty (ZeroOne Double, a))
  deriving (Show, Generic)

-- | A fixed rate schedule.
newtype FixedRate = FixedRate Double
  deriving (Show, Generic)

-- | A rate schedule decreasing with \(1/\sqrt{t}\)
newtype InverseSqrtRate = InverseSqrtRate Double
  deriving (Show, Generic)

-- | The inverse of the Lagrange-Fenchel function of
-- \(\phi(\Lambda) = \Lambda^2 / 8 \). Together with \(\Alpha=4\),
-- this is the hyperparameter for UCB1.
data AlphaUCBInvLFPhi = AlphaUCBInvLFPhi
  deriving (Show, Generic)
