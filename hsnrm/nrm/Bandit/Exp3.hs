{-# LANGUAGE DeriveAnyClass #-}
{-# LANGUAGE DerivingVia #-}

{-|
Module      : Bandit.Exp3
Copyright   : (c) 2019, UChicago Argonne, LLC.
License     : MIT
Maintainer  : fre@freux.fr

The exponential-weight algorithm for Exploration and Exploitation (EXP3). See [1]

- [1] Regret Analysis of Stochastic and Nonstochastic Multi-armed Bandit Problems,
  Sebastien Bubeck and Nicolo Cesa-Bianchi. http://arxiv.org/abs/1204.5721
-}
module Bandit.Exp3
  ( -- * State
    Exp3 (..)
  , -- * Internal
    Weight (..)
  , Probability (..)
  , CumulativeLoss (..)
  )
where

import Bandit.Class
import Control.Lens
import Data.Generics.Product
import Data.Random
import qualified Data.Random.Distribution.Categorical as DC
import qualified Data.Random.Sample as RS
import Protolude
import qualified Refined as R

-- | The EXP3 state
data Exp3 a
  = Exp3
      { t :: Int
      , lastAction :: a
      , k :: Int
      , weights :: NonEmpty (Weight a)
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
      { probability :: Probability
      , cumulativeLoss :: CumulativeLoss
      , action :: a
      }
  deriving (Generic)

newtype ZeroOneInterval l = ZeroOneInterval (R.Refined (R.FromTo 0 1) l)

unrefineLoss :: ZeroOneInterval l -> l
unrefineLoss (ZeroOneInterval l) = R.unrefine l

_weights :: Data.Generics.Product.HasField "weights" s t a b => Lens s t a b
_weights = field @"weights"

instance
  (Eq a)
  => Bandit (Exp3 a) (Arms a) a (ZeroOneInterval Double) where

  init (Arms as) = do
    a <- RS.sample . DC.fromWeightedList $ toList as <&> (1 :: Double,)
    let ws = as <&> Weight (Probability 1) (CumulativeLoss 0)
    return $
      ( Exp3
          { t = 1
          , lastAction = a
          , k = length as
          , weights = ws
          }
      , a
      )

  step (unrefineLoss -> l) =
    get <&> lastAction >>= \oldAction -> do
      _weights %=
        fmap (\w -> if action w == oldAction then updateCumLoss l w else w)
      t <- use $ field @"t"
      k <- use $ field @"k"
      _weights %= recompute t k
      field @"t" += 1
      pickAction

pickAction :: (MonadRandom m, MonadState (Exp3 a) m) => m a
pickAction = get >>= s >>= btw (assign (field @"lastAction"))
  where
    s bandit = RS.sample . DC.fromWeightedList $ toList (weights bandit) <&> w2tuple
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

btw :: (Functor f) => (t -> f b) -> t -> f t
btw k x = x <$ k x

-- | Regret bound for this \(\mathbb{L}=[0,1]\)-loss hyperparameter-free EXP3 version:
-- \[
-- R_T \leq \sqrt{2 T K \ln K}
-- \]
instance (Eq a) => ParameterFreeMAB (Exp3 a) a (ZeroOneInterval Double)
