{-# LANGUAGE DerivingVia #-}

-- |
-- Module      : HBandit.BwCR
-- Copyright   : (c) 2019, UChicago Argonne, LLC.
-- License     : MIT
-- Maintainer  : fre@freux.fr
--
-- Bandits with convex knapsacks and concave rewards (BwCR). See [1]
--
-- The parameters from the paper are:
-- Convex set \(S \in [0,1]^d\).
-- Convex loss function \(f : [0,1]^d -> [0,1]\) (written as concave objective in the paper).
-- \(m\) arms.
--
-- Here, we implement the special LP case.
-- This means that the case of \(k\) linearly-combined rewards
-- (with weights \(\lambda\)) and \(c\) constraints (so that \(d = c+k\)) is
-- implemented.
-- More precisely, \(f(X) = \sum_{i=1..k} \lambda_i X_i\) and
-- \(S = [0,1]^k \times [a_1,b_1] \times \ldots \times [a_c,b_c]\).
--
-- In this restricted case, the minimization(maximization in the original paper)
-- inside the bandit iteration is a LP with k variables and c constraints. This
-- LP is solved via GLPK bindings.
--
-- - [1] Bandits with Global Convex Constraints and Objective,
--   Shipra Agrawal, Nikhil R.Devanur. https://pubsonline.informs.org/doi/abs/10.1287/opre.2019.1840
module HBandit.BwCR
  ( BwCR (..),
    BwCRHyper (..),
    ScreeningBwCR (..),
    UCBBwCR (..),
    Weight (..),
    T (..),
    mkHyper,
  )
where

import HBandit.Class
import HBandit.Types
import Numeric.Interval
import Protolude

--import System.Random

newtype T = T Double deriving (Generic, Eq)

-- | The BwCR state
data BwCR a l
  = -- | Still screening for initial estimates
    Screening (ScreeningBwCR a l)
  | -- | The UCB sampling procedure has started.
    UCB (UCBBwCR a l)
  deriving (Generic)

data ScreeningBwCR a l
  = ScreeningBwCR
      { tScreening :: Int,
        screening :: a,
        screened :: [([ZeroOne Double], a)],
        screenQueue :: [a],
        screeningHyper :: BwCRHyper a l
      }
  deriving (Generic)

-- | The information maintaining structure for one action.
data Weight a
  = Weight
      { cumulativeEstimates :: [ZeroOne Double],
        hits :: Int,
        action :: a
      }
  deriving (Generic)

data UCBBwCR a l
  = UCBBwCR
      { t :: Int,
        lastAction :: a,
        k :: Int,
        weights :: NonEmpty (Weight a),
        ucbHyper :: BwCRHyper a l
      }
  deriving (Generic)

-- | @mkHyper@ \(\delta\) builds the hyperparameter for the original paper
-- bound with confidence interval in probability \(1-\delta\).
mkHyper ::
  Double ->
  T ->
  Arms a ->
  Interval [ZeroOne Double] ->
  [ZeroOne Double] ->
  BwCRHyper a [ZeroOne Double]
mkHyper delta (T totalRounds) arms@(Arms nelarms) constraints objectiveWeights =
  BwCRHyper
    { gamma = log (totalRounds * fromIntegral (length $ toList nelarms) * fromIntegral (length objectiveWeights) / delta),
      ..
    }

-- | the BwCR hyperparameter \(\gamma\) used in \(rad_{\gamma(\nu,N)}\)
-- used in the original paper.
data BwCRHyper a l
  = BwCRHyper
      { gamma :: Double,
        arms :: Arms a,
        constraints :: Interval l,
        objectiveWeights :: l
      }
  deriving (Generic)

instance Bandit (BwCR a [ZeroOne Double]) (BwCRHyper a [ZeroOne Double]) a [ZeroOne Double] where

  init g h@(BwCRHyper _ (Arms (a :| as)) _ _) =
    ( Screening $ ScreeningBwCR
        { tScreening = 1,
          screeningHyper = h,
          screening = a,
          screened = [],
          screenQueue = as
        },
      a,
      g
    )

  step g l =
    get >>= \case
      Screening sg ->
        case screenQueue sg of
          (a : as) -> do
            put . Screening $
              sg
                { tScreening = tScreening sg + 1,
                  screening = a,
                  screened = (l, screening sg) : screened sg,
                  screenQueue = as
                }
            return (a, g)
          [] -> do
            let eeg = UCBBwCR
                  { t = tScreening sg + 1,
                    ucbHyper = screeningHyper sg,
                    lastAction = screening sg,
                    k = length (screened sg) + 1,
                    weights = toW <$> ((l, screening sg) :| screened sg)
                  }
            pickreturn eeg g
            where
              toW :: ([ZeroOne Double], a) -> Weight a
              toW (cumulativeEstimates, action) = Weight {hits = 1, ..}
      UCB s -> pickreturn s g

-- | Action selection primitive
pickAction :: UCBBwCR a l -> a
pickAction (UCBBwCR _ lastAction _ _ _) = lastAction

-- | Action selection and  return
pickreturn ::
  (MonadState (BwCR a l) m) =>
  UCBBwCR a l ->
  g ->
  m (a, g)
pickreturn eeg g = do
  let a = pickAction eeg
  put $ UCB $ eeg {lastAction = a}
  return (a, g)

-- | The BwCR algorithm with a linear combination as objective. Under IID data sources,
-- This offers for any \(\delta \geq 0 \) the regret bounds in probability \(1-\delta\):
-- \[
-- R_{objective}(T) = O (\|\lambda\| d \sqrt{\frac{\gamma m}{T}} )
-- \]
-- \[
-- R_{constraints}(T) = O ( d \sqrt{\frac{\gamma m}{T}} )
-- \]
-- Where \(\gamma = O (\log(\frac{mTd}{\delta}))\).
instance (Eq a) => BwCRMAB (BwCR a [ZeroOne Double]) a (BwCRHyper a) [ZeroOne Double]
