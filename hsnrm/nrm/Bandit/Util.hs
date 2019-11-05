-- |
-- Module      : Bandit.Util
-- Copyright   : (c) 2019, UChicago Argonne, LLC.
-- License     : MIT
-- Maintainer  : fre@freux.fr
--
-- The exponential-weight algorithm for Exploration and Exploitation (EXP3). See [1]
--
-- - [1] Regret Analysis of Stochastic and Nonstochastic Multi-armed Bandit Problems,
--   Sebastien Bubeck and Nicolo Cesa-Bianchi. http://arxiv.org/abs/1204.5721
module Bandit.Util
  ( sampleWL,
  )
where

import Control.Monad.Random
import Protolude

sampleWL :: RandomGen g => [(Double, a)] -> g -> (a, g)
sampleWL weights = runRand (fromList (weights <&> \(r, x) -> (x, toRational r)))
