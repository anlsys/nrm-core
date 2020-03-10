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
    Exp4RCfg (..),
    Feedback (..),
    LastAction(..),
    mkMu,
    mkDelta,
    lambdaInitial,
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
import qualified Refined.Unsafe as R

-- | The EXP4R state
data Exp4R s a
  = Exp4R
      { t :: Int,
        horizon :: R.Refined R.Positive Int,
        lastAction :: Maybe (LastAction a),
        k :: Int,
        n :: Int,
        lambda :: R.Refined R.NonNegative Double,
        constraint :: ZeroOne Double,
        experts ::
          NonEmpty
            ( ZeroOne Double,
              s -> NonEmpty (ZeroOne Double, a)
            )
      }
  deriving (Generic)

data LastAction a
  = LastAction
      { action :: a,
        globalProbabilityOfSample :: ZeroOne Double,
        perExpertProbabilityOfSample :: NonEmpty (ZeroOne Double)
      }
  deriving (Generic)

data Feedback
  = Feedback
      { cost :: ZeroOne Double,
        risk :: ZeroOne Double
      }

data Exp4RCfg s a
  = Exp4RCfg
      { expertsCfg :: NonEmpty (s -> NonEmpty (ZeroOne Double, a)),
        constraintCfg :: ZeroOne Double,
        horizonCfg :: R.Refined R.Positive Int,
        as :: NonEmpty a
      }
  deriving (Generic)

instance
  (Eq a) =>
  ContextualBandit (Exp4R s a) (Exp4RCfg s a) s a (Maybe Feedback)
  where

  initCtx Exp4RCfg {..} =
    Exp4R
      { t = 1,
        lastAction = Nothing,
        k = NE.length as,
        n = NE.length expertsCfg,
        lambda = lambdaInitial,
        constraint = constraintCfg,
        horizon = horizonCfg,
        experts = (R.unsafeRefine (1 / fromIntegral (NE.length expertsCfg)),) <$> expertsCfg
      }

  stepCtx g feedback s =
    do
      weightedExperts <- use (field @"experts")
      lam <- R.unrefine <$> use (field @"lambda")
      beta <- use (field @"constraint")
      mu <- get <&> mkMu
      delta <- get <&> mkDelta
      use (field @"lastAction") >>= \case
        Nothing -> return ()
        Just (LastAction _ p_a pPolicy_a) -> feedback & \case
          Nothing -> panic "exp4R usage error: don't give feedback on first action."
          Just f -> do
            let cHat :: Double
                cHat = R.unrefine (cost f) / R.unrefine p_a
                rHat :: Double
                rHat = R.unrefine (risk f) / R.unrefine p_a
                yHats :: NonEmpty Double
                yHats = (\p -> cHat * R.unrefine p) <$> pPolicy_a
                zHats :: NonEmpty Double
                zHats = (\p -> rHat * R.unrefine p) <$> pPolicy_a
                wOld :: NonEmpty (ZeroOne Double)
                wOld = fst <$> weightedExperts
                expTerms :: NonEmpty Double
                expTerms = NE.zipWith (\y z -> y + lam * z) yHats zHats
                wUpdate = NE.zipWith (\(R.unrefine -> w) x -> w * exp (- mu * x)) wOld expTerms
                wDenom = getSum $ sconcat $ Sum <$> wUpdate
            field @"experts" .= NE.zipWith (\(_, e) w' -> (unsafeNormalizePanic w' wDenom, e)) weightedExperts wUpdate
            field @"lambda" .= R.unsafeRefine (max 0 (lam + mu * (((R.unrefine <$> wOld) `neDot` zHats) - R.unrefine beta - delta * mu * lam)))
      let weightedAdviceMatrix :: NonEmpty (ZeroOne Double, NonEmpty (ZeroOne Double, a))
          weightedAdviceMatrix = weightedExperts <&> \(wi, pi_i) -> (wi, pi_i s)
          dirtyArmDistribution :: NonEmpty (Double, a)
          dirtyArmDistribution = sconcat $ weightedAdviceMatrix <&> \(wi, advices) -> advices <&> \(p, ai) -> (R.unrefine p * R.unrefine wi, ai)
          dirtyarmDistribution' :: NonEmpty (Double, a)
          dirtyarmDistribution' = groupAllWith1 snd dirtyArmDistribution <&> \gs -> (getSum $ sconcat (gs <&> Sum . fst), snd $ NE.head gs)
          armDistribution :: NonEmpty (ZeroOne Double, a)
          armDistribution = normalizeDistribution dirtyarmDistribution' & \case
            Nothing -> panic "internal Exp4R algorithm failure: distribution normalization failed."
            Just d -> d
          (a, g') = sampleWL armDistribution g
          p_a = find (\x -> snd x == a) armDistribution & \case
            Nothing -> panic "internal Exp4R algorithm failure: arm pull issue."
            Just p -> fst p
          probabilityOf_a :: NonEmpty (ZeroOne Double)
          probabilityOf_a = snd <$> weightedAdviceMatrix
            <&> \e ->
              case find (\x -> snd x == a) e of
                Nothing -> panic "internal Exp4R algorithm failure: weight computation"
                Just (p, _) -> p
      field @"lastAction" ?= LastAction a p_a probabilityOf_a
      return (a, g')

neDot :: (Num a) => NonEmpty a -> NonEmpty a -> a
neDot x y = getSum $ sconcat (Sum <$> NE.zipWith (*) x y)

-- | \( \mu = \sqrt{\frac{\ln N }{ (T(K+4))}} \)
mkMu :: Exp4R s a -> Double
mkMu Exp4R {..} =
  sqrt $ log (fromIntegral n) / fromIntegral (R.unrefine horizon * (k + 4))

-- | \( \delta = 3K \)
mkDelta :: Exp4R s a -> Double
mkDelta Exp4R {..} = fromIntegral $ 3 * k

-- | \( \lambda_1 = 0 \)
lambdaInitial :: R.Refined R.NonNegative Double
lambdaInitial = R.unsafeRefine 0
