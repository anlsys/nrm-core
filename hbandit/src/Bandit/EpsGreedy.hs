{-# OPTIONS_GHC -fno-warn-partial-fields #-}

-- |
-- Module      : Bandit.EpsGreedy
-- Copyright   : (c) 2019, UChicago Argonne, LLC.
-- License     : MIT
-- Maintainer  : fre@freux.fr
--
-- This module implements the fixed rate \(\epsilon\)-Greedy MAB algorithm.
--
-- The \(\epsilon\)-Greedy algorithm selects a random action with
-- probability \(\epsilon\), or select the action with best average
-- with probability \(1-\epsilon\).
module Bandit.EpsGreedy
  ( EpsGreedy (..),
    Weight (..),
    EpsGreedyHyper (..),
    pickRandom,
    updateAvgLoss,
  )
where

import Bandit.Class
import Bandit.Types
import Bandit.Util
import Control.Lens
import Control.Monad.Random as MR (fromList, runRand)
import Data.Generics.Labels ()
import Protolude
import System.Random

data EpsGreedy a r
  = EpsGreedy
      { t :: Int,
        rate :: r,
        lastAction :: a,
        params :: Params a
      }
  deriving (Show, Generic)

data Params a = InitialScreening (Screening a) | Started (ExploreExploit a)
  deriving (Show, Generic)

-- | Still screening for initial estimates
data Screening a
  = Screening
      { screened :: [(Double, a)],
        screenQueue :: [a]
      }
  deriving (Show, Generic)

-- | The sampling procedure has started.
data ExploreExploit a
  = ExploreExploit
      { k :: Int,
        weights :: NonEmpty (Weight a)
      }
  deriving (Show, Generic)

-- | The information maintaining structure for one action.
data Weight a
  = Weight
      { averageLoss :: Double,
        hits :: Int,
        action :: a
      }
  deriving (Show)
  deriving (Generic)

toW :: (Double, a) -> Weight a
toW (loss, action) = Weight loss 1 action

-- | The epsilon-greedy hyperparameter.
data EpsGreedyHyper a r
  = EpsGreedyHyper
      { rateRep :: r,
        arms :: Arms a
      }
  deriving (Show)

-- | The variable rate \(\epsilon\)-Greedy MAB algorithm.
-- Offers no interesting guarantees, works well in practice.
instance (Rate r, Eq a) => Bandit (EpsGreedy a r) (EpsGreedyHyper a r) a Double where

  init g (EpsGreedyHyper r (Arms (a :| as))) =
    ( EpsGreedy
        { t = 1,
          rate = r,
          lastAction = a,
          params = InitialScreening $ Screening
            { screened = [],
              screenQueue = as
            }
        },
      a,
      g
    )

  step g l = do
    oldAction <- use #lastAction
    schedule <- use #rate <&> toRate
    e <- use #t <&> schedule
    #t += 1
    (a, newGen) <- use #params >>= \case
      InitialScreening sg ->
        case screenQueue sg of
          (a : as) -> do
            #params . #_InitialScreening .= Screening
              { screened = (l, oldAction) : screened sg,
                screenQueue = as
              }
            return (a, g)
          [] -> do
            let ee = ExploreExploit
                  { k = length (screened sg) + 1,
                    weights = toW <$> ((l, oldAction) :| screened sg)
                  }
            #params . #_Started .= ee
            pickreturn e g ee
      Started s -> do
        let eeg =
              s
                { weights = weights s <&> \w ->
                    if action w == oldAction
                      then updateAvgLoss l w
                      else w
                }
        pickreturn e g eeg
    #lastAction .= a
    return (a, newGen)

-- | Action selection and  return
pickreturn ::
  (RandomGen g, MonadState (EpsGreedy b r) m) =>
  Double ->
  g ->
  ExploreExploit b ->
  m (b, g)
pickreturn eps g eeg = do
  let (a, g') = runRand (MR.fromList [(True, toRational eps), (False, toRational $ 1 - eps)]) g & \case
        (True, g'') -> pickRandom eeg g''
        (False, g'') -> (action $ minimumBy (\(averageLoss -> a1) (averageLoss -> a2) -> compare a1 a2) (weights eeg), g'')
  return (a, g')

-- | Random action selection primitive
pickRandom :: (RandomGen g) => ExploreExploit a -> g -> (a, g)
pickRandom ExploreExploit {..} =
  sampleWL $
    fromMaybe
      (panic "distribution normalization failure")
      (normalizeDistribution $ weights <&> w2tuple)
  where
    w2tuple :: Weight b -> (Double, b)
    w2tuple (Weight _avgloss _hits action) = (1, action)

-- | rudimentary online mean accumulator.
updateAvgLoss :: Double -> Weight a -> Weight a
updateAvgLoss l (Weight avgloss hits action) =
  Weight
    ( (avgloss * fromIntegral hits + l)
        / (fromIntegral hits + 1)
    )
    (hits + 1)
    action
