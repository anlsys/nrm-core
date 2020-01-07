-- |
-- Module      : Bandit.Util
-- Copyright   : (c) 2019, UChicago Argonne, LLC.
-- License     : MIT
-- Maintainer  : fre@freux.fr
--
-- Utility functions for MAB algorithms.
module Bandit.Util
  ( sampleWL,
  )
where

import Control.Monad.Random
import Protolude

-- | Samples from a weighted probability distribution.
-- The sum of weights must not be zero.
sampleWL :: RandomGen g => [(Double, a)] -> g -> (a, g)
sampleWL weights = runRand (fromList (weights <&> \(r, x) -> (x, toRational r)))
