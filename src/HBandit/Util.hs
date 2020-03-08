-- |
-- Module      : HBandit.Util
-- Copyright   : (c) 2019, UChicago Argonne, LLC.
-- License     : MIT
-- Maintainer  : fre@freux.fr
--
-- Utility functions for MAB algorithms.
module HBandit.Util
  ( sampleWL,
  )
where

import Control.Monad.Random
import Protolude

-- | Samples from a weighted probability distribution.
-- The sum of weights must not be zero.
sampleWL :: RandomGen g => NonEmpty (Double, a) -> g -> (a, g)
sampleWL weights = runRand (fromList $ toList (weights <&> \(r, x) -> (x, toRational r)))
