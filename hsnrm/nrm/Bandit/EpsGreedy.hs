{-|
Module      : Bandit.EpsGreedy
Copyright   : (c) 2019, UChicago Argonne, LLC.
License     : MIT
Maintainer  : fre@freux.fr
-}
module Bandit.EpsGreedy
  ( EpsGreedy (..)
  , Weight (..)
  , EpsGreedyHyper (..)
  , ScreeningGreedy (..)
  , ExploreExploitGreedy (..)
  )
where

import Bandit.Class
{-import qualified Data.List.NonEmpty as NE-}
import Data.Random
import qualified Data.Random.Distribution.Categorical as DC
import qualified Data.Random.Sample as RS
import Protolude

data EpsGreedy a
  = Screening (ScreeningGreedy a)
  | ExploreExploit (ExploreExploitGreedy a)

data ScreeningGreedy a
  = ScreeningGreedy
      { tScreening :: Int
      , epsScreening :: Double
      , screening :: a
      , screened :: [(Double, a)]
      , screenQueue :: [a]
      }

data ExploreExploitGreedy a
  = ExploreExploitGreedy
      { t :: Int
      , eps :: Double
      , lastAction :: a
      , k :: Int
      , weights :: NonEmpty (Weight a)
      }

-- | EpsGreedy data structure for one action
data Weight a
  = Weight
      { averageLoss :: Double
      , hits :: Int
      , action :: a
      }
  deriving (Generic)

data EpsGreedyHyper a
  = EpsGreedyHyper
      { epsilon :: Double
      , arms :: Arms a
      }

instance (Eq a) => Bandit (EpsGreedy a) (EpsGreedyHyper a) a Double where

  init (EpsGreedyHyper e (Arms (a :| as))) =
    return $
      ( Screening $ ScreeningGreedy
          { tScreening = 1
          , epsScreening = e
          , screening = a
          , screened = []
          , screenQueue = as
          }
      , a
      )

  step l =
    get >>= \case
      Screening sg ->
        case screenQueue sg of
          (a : as) -> do
            put $ Screening $
              sg
                { tScreening = tScreening sg + 1
                , screening = a
                , screened = (l, screening sg) : screened sg
                , screenQueue = as
                }
            return a
          [] -> do
            let eeg = ExploreExploitGreedy
                  { t = tScreening sg + 1
                  , eps = epsScreening sg
                  , lastAction = screening sg
                  , k = length (screened sg) + 1
                  , weights = toW <$> ((l, screening sg) :| screened sg)
                  }
            a <- pickAction eeg
            put $ ExploreExploit $ eeg {lastAction = a}
            return a
            where
              toW (loss, action) = Weight loss 1 action
      ExploreExploit s -> do
        let eeg =
              s
                { t = t s + 1
                , weights = weights s <&> \w ->
                  if action w == lastAction s
                  then updateAvgLoss l w
                  else w
                }
        a <- pickAction eeg
        put $ ExploreExploit $ eeg {lastAction = a}
        return a

pickAction :: (MonadRandom m) => ExploreExploitGreedy a -> m a
pickAction ExploreExploitGreedy {..} =
  RS.sample . DC.fromWeightedList $ toList $ weights <&> w2tuple
  where
    w2tuple (Weight avgloss _hits action) = (avgloss, action)

-- | TODO improve numerical resiliency.
updateAvgLoss :: Double -> Weight a -> Weight a
updateAvgLoss l (Weight avgloss hits action) =
  Weight
    ((avgloss * fromIntegral hits + l) / (fromIntegral hits + 1))
    (hits + 1)
    action
