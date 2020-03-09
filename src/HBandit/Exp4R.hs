{-# LANGUAGE DerivingVia #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE TypeApplications #-}

-- |
-- Module      : HBandit.Exp4R
-- Copyright   : (c) 2019, UChicago Argonne, LLC.
-- License     : MIT
-- Maintainer  : fre@freux.fr
--
-- The contextual exponential-weight algorithm for Exploration and Exploitation
-- with Experts and Risk Constraints (EXP4R). See [1]
--
-- - [1] Sun, W., Dey, D. & Kapoor, A.. (2017). Safety-Aware Algorithms for
-- Adversarial Contextual Bandit. Proceedings of the 34th International
-- Conference on Machine Learning, in PMLR 70:3280-3288
module HBandit.Exp4R
  ( -- * State
    Exp4R (..),
  )
where

import Control.Lens
import Data.Generics.Product
import Data.List.NonEmpty as NE
import HBandit.Class
import HBandit.Types
import HBandit.Util
import Protolude
import qualified Refined as R
import System.Random

-- | The EXP3 state
data Exp4R s a
  = Exp4R
      { t :: Int,
        lastAction :: a,
        k :: Int,
        lambda :: R.Refined R.Positive Double,
        experts :: NonEmpty (R.Refined R.Positive Double, s -> NonEmpty (ZeroOne Double, a))
      }
  deriving (Generic)

instance
  (Eq a) =>
  ContextualBandit (Exp4R s a) (NonEmpty (s -> NonEmpty (ZeroOne Double, a))) s a (ZeroOne Double, ZeroOne Double)
  where

  initCtx g (cfgExperts) = undefined

  stepCtx g ((R.unrefine -> c, R.unrefine -> r)) s = do
    weightedAdvices <- use (field @"experts") <&> fmap (fmap (\exp -> exp s))
    let p_t :: NonEmpty (Double, a)
        p_t = NE.groupAllWith1 snd weightedAdvices
          <&> \gs ->
            ( getSum $ sconcat (gs <&> Sum . R.unrefine . fst),
              snd $ NE.head gs
            )
    let (a, g') = sampleWL p_t g
    return undefined
