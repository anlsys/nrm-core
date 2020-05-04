{-# LANGUAGE DerivingVia #-}

-- |
-- Module      : HBandit.Exp3
-- Copyright   : (c) 2019, UChicago Argonne, LLC.
-- License     : MIT
-- Maintainer  : fre@freux.fr
--
-- The exponential-weight algorithm for Exploration and Exploitation (EXP3). See [1]
--
-- - [1] Regret Analysis of Stochastic and Nonstochastic Multi-armed Bandit Problems,
--   Sebastien Bubeck and Nicolo Cesa-Bianchi. http://arxiv.org/abs/1204.5721
module HBandit.Exp3
  ( -- * State
    Exp3 (..),

    -- * Internal
    Weight (..),
    Probability (..),
    CumulativeLoss (..),
  )
where

import Control.Lens hiding (_Unwrapped)
import Data.Generics.Labels ()
import Data.Generics.Wrapped
import HBandit.Class
import HBandit.Types
import HBandit.Util
import Protolude
import qualified Refined as R
import System.Random

-- | The EXP3 state
data Exp3 a
  = Exp3
      { t :: Int,
        lastAction :: a,
        k :: Int,
        weights :: NonEmpty (Weight a)
      }
  deriving (Generic)

-- | Probability of picking an action
newtype Probability = Probability {getProbability :: Double}
  deriving (Generic)

-- | Cumulative loss counter for an action
newtype CumulativeLoss = CumulativeLoss {getCumulativeLoss :: Double}
  deriving (Generic)

-- | Exp3 weight for one action
data Weight a
  = Weight
      { probability :: Probability,
        cumulativeLoss :: CumulativeLoss,
        action :: a
      }
  deriving (Generic)

-- | The Exponential-weight algorithm for Exploration and Exploitation (EXP3).
instance
  (Eq a) =>
  Bandit (Exp3 a) (Arms a) a (ZeroOne Double)
  where

  init g (Arms as) =
    ( Exp3
        { t = 1,
          lastAction = a,
          k = length as,
          weights = ws
        },
      a,
      g'
    )
    where
      awl = as <&> (HBandit.Types.one,)
      (a, g') = sampleWL awl g
      ws = as <&> Weight (Probability $ 1.0 / fromIntegral (length (toList as))) (CumulativeLoss 0)

  step g (R.unrefine -> l) = do
    oldAction <- use #lastAction
    #weights %= fmap (\w -> if action w == oldAction then updateCumLoss l w else w)
    t <- use #t
    k <- use #k
    #weights %= recompute t k
    #t += 1
    pickAction g

pickAction :: (RandomGen g, MonadState (Exp3 a) m) => g -> m (a, g)
pickAction g = do
  bandit <- get
  let (a, g') =
        sampleWL
          ( normalizeDistribution (weights bandit <&> w2tuple) & \case
              Nothing -> panic "Exp3 internal distribution normalization error"
              Just x -> x
          )
          g
  #lastAction .= a
  return (a, g')
  where
    w2tuple :: forall b. Weight b -> (Double, b)
    w2tuple (Weight p _ action) = (getProbability p, action)

updateCumLoss :: Double -> Weight a -> Weight a
updateCumLoss l w@(Weight (Probability p) _ _) =
  w & #cumulativeLoss . _Unwrapped +~ l / p

recompute :: Int -> Int -> NonEmpty (Weight a) -> NonEmpty (Weight a)
recompute t k weights = updatep <$> weights
  where
    updatep w@(Weight _ (CumulativeLoss cL) _) =
      w & #probability . #getProbability
        .~ expw cL
        / denom
    expw cL =
      exp (- sqrt (2.0 * log (fromIntegral k) / fromIntegral (t * k)) * cL)
    denom = getSum $ foldMap denomF weights
    denomF (getCumulativeLoss . cumulativeLoss -> cL) = Sum $ expw cL

-- | Regret bound for this \(\mathbb{L}=[0,1]\)-loss hyperparameter-free EXP3 version:
-- \[
-- R_T \leq \sqrt{2 T K \ln K}
-- \]
instance (Eq a) => ParameterFreeMAB (Exp3 a) a (ZeroOne Double)
