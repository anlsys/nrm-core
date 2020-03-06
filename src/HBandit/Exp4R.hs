{-# LANGUAGE DerivingVia #-}
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
    Exp4R (..)
  , -- * Internal
    Weight (..)
  , Probability (..)
  , CumulativeLoss (..)
  )
where

import Control.Lens
import Data.Generics.Product
import HBandit.Class
import HBandit.Types
import HBandit.Util
import Protolude
import qualified Refined as R
import System.Random

-- | The EXP3 state
data Exp4R s a
  = Exp4R
      { t :: Int
      , lastAction :: a
      , k :: Int
      , lambda :: R.Refined R.Positive Double
      , experts :: NonEmpty (R.Refined R.Positive Double, s -> a)
      }
  deriving (Generic)

instance
  (Eq a)
  => Bandit (Exp4R a) (NonEmpty (s-> a)) a (ZeroOne Double) where

  init g (cfgExperts) =
    ( Exp4R
        { t = 1
        , lastAction = a

        , experts = (1/length as,) <$> cfgExperts
        }
    , a
    , g'
    )
    where
      awl = toList as <&> (1 :: Double,)
      (a, g') = sampleWL awl g
      ws = as <&> Weight (Probability $ 1.0 / fromIntegral (length (toList as))) (CumulativeLoss 0)

  step g (R.unrefine -> l) =
    get <&> lastAction >>= \oldAction -> do
      _weights %=
        fmap (\w -> if action w == oldAction then updateCumLoss l w else w)
      t <- use $ field @"t"
      k <- use $ field @"k"
      _weights %= recompute t k
      field @"t" += 1
      pickAction g

pickAction :: (RandomGen g, MonadState (Exp4R a) m) => g -> m (a, g)
pickAction g = do
  bandit <- get
  let (a, g') = sampleWL (toList (weights bandit) <&> w2tuple) g
  field @"lastAction" .= a
  return (a, g')
  where
    w2tuple :: forall b. Weight b -> (Double, b)
    w2tuple (Weight p _ action) = (getProbability p, action)

updateCumLoss :: Double -> Weight a -> Weight a
updateCumLoss l w@(Weight (Probability p) (CumulativeLoss cL) _) =
  w & field @"cumulativeLoss" .~ CumulativeLoss (cL + (l / p))

recompute :: Int -> Int -> NonEmpty (Weight a) -> NonEmpty (Weight a)
recompute t k weights = updatep <$> weights
  where
    updatep w@(Weight _ (CumulativeLoss cL) _) =
      w & field @"probability" . field @"getProbability" .~
        expw cL /
        denom
    expw cL =
      exp (- sqrt (2.0 * log (fromIntegral k) / fromIntegral (t * k)) * cL)
    denom = getSum $ foldMap denomF weights
    denomF (getCumulativeLoss . cumulativeLoss -> cL) = Sum $ expw cL

-- | Regret bound for this \(\mathbb{L}=[0,1]\)-loss hyperparameter-free EXP3 version:
-- \[
-- R_T \leq \sqrt{2 T K \ln K}
-- \]
instance (Eq a) => ParameterFreeMAB (Exp4R a) a (ZeroOne Double)
