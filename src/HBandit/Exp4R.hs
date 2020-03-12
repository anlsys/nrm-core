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
  ( -- * Interface
    Feedback (..),

    -- * State
    Exp4R (..),
    LastAction (..),

    -- * Configuration
    Exp4RCfg (..),

    -- * Experts
    ObliviousRep (..),

    -- * internal
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
data Exp4R s a er
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
              er
            )
      }
  deriving (Generic)

-- | Encapsulator for 'last action taken'
data LastAction a
  = LastAction
      { action :: a,
        globalProbabilityOfSample :: ZeroOne Double,
        perExpertProbabilityOfSample :: NonEmpty (ZeroOne Double)
      }
  deriving (Generic)

-- | Constructor for feedback from the environment.
data Feedback
  = Feedback
      { cost :: ZeroOne Double,
        risk :: ZeroOne Double
      }
  deriving (Generic)

-- | Hyperparameters.
data Exp4RCfg s a er
  = Exp4RCfg
      { expertsCfg :: NonEmpty er,
        constraintCfg :: ZeroOne Double,
        horizonCfg :: R.Refined R.Positive Int,
        as :: NonEmpty a
      }
  deriving (Generic)

instance
  (Eq a, ExpertRepresentation er s a) =>
  ContextualBandit (Exp4R s a er) (Exp4RCfg s a er) s a (Maybe Feedback) er
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

  stepCtx g feedback s = do
    weightedExperts <- use (field @"experts") <&> fmap (fmap represent)
    lam <- R.unrefine <$> use (field @"lambda")
    beta <- use (field @"constraint")
    mu <- get <&> mkMu
    delta <- get <&> mkDelta
    use (field @"lastAction")
      >>= traverse_ \(LastAction _ (R.unrefine -> p_a) (fmap R.unrefine -> pPolicy_a)) ->
        fromMaybe
          (panic "exp4R usage error: do not give feedback on first action.")
          ( feedback
              <&> \(Feedback (R.unrefine -> c) (R.unrefine -> r)) -> do
                let numeratorTerm (R.unrefine -> w, _) p = w * exp (- mu * (p * (lam * r + c) / p_a))
                let wUpdate = NE.zipWith numeratorTerm weightedExperts pPolicy_a
                    wDenom = getSum $ sconcat $ Sum <$> wUpdate
                field @"experts" %= NE.zipWith (\w' (_, e) -> (unsafeNormalizePanic w' wDenom, e)) wUpdate
                let fDot (R.unrefine -> wi, _) p = wi * r * p / p_a
                let dotted = getSum $ sconcat (Sum <$> NE.zipWith fDot weightedExperts pPolicy_a)
                field @"lambda" .= R.unsafeRefine (max 0 (lam + mu * (dotted - R.unrefine beta - delta * mu * lam)))
          )
    let weightedAdviceMatrix :: NonEmpty (ZeroOne Double, NonEmpty (ZeroOne Double, a))
        weightedAdviceMatrix = weightedExperts <&> fmap ($ s)
        armDistribution :: NonEmpty (ZeroOne Double, a)
        armDistribution =
          fromMaybe
            (panic "internal Exp4R algorithm failure: distribution normalization failed.")
            (combineAdvice weightedAdviceMatrix)
        (a, g') = sampleWL armDistribution g
        p_a =
          fst $
            fromMaybe
              (panic "internal Exp4R algorithm failure: arm pull issue.")
              (find (\x -> snd x == a) armDistribution)
        probabilityOf_a :: NonEmpty (ZeroOne Double)
        probabilityOf_a = snd <$> weightedAdviceMatrix
          <&> \e ->
            ( fst $
                fromMaybe
                  (panic "internal Exp4R algorithm failure: weight computation")
                  (find (\x -> snd x == a) e)
            )
    field @"lastAction" ?= LastAction a p_a probabilityOf_a
    return (a, g')

-- | combineAdvice turns weighted expert advice into a probability distribution to
-- sample from.
combineAdvice ::
  (Ord a) =>
  NonEmpty (ZeroOne Double, NonEmpty (ZeroOne Double, a)) ->
  Maybe (NonEmpty (ZeroOne Double, a))
combineAdvice weightedAdviceMatrix = normalizeDistribution $
  groupAllWith1 snd dirtyArmDistribution
    <&> \gs -> (getSum $ sconcat (gs <&> Sum . fst), snd $ NE.head gs)
  where
    dirtyArmDistribution = sconcat $
      weightedAdviceMatrix
        <&> \(wi, advices) -> advices <&> \(p, ai) -> (R.unrefine p * R.unrefine wi, ai)

-- | \( \mu = \sqrt{\frac{\ln N }{ (T(K+4))}} \)
mkMu :: Exp4R s a er -> Double
mkMu Exp4R {..} =
  sqrt $ log (fromIntegral n) / fromIntegral (R.unrefine horizon * (k + 4))

-- | \( \delta = 3K \)
mkDelta :: Exp4R s a er -> Double
mkDelta Exp4R {..} = fromIntegral $ 3 * k

-- | \( \lambda_1 = 0 \)
lambdaInitial :: R.Refined R.NonNegative Double
lambdaInitial = R.unsafeRefine 0

-- | Oblivious Categorical Expert Representation
data ObliviousRep a = ObliviousRep (NonEmpty (ZeroOne Double, a)) deriving (Generic)

instance ExpertRepresentation (ObliviousRep a) () a where
  represent (ObliviousRep l) () = l
