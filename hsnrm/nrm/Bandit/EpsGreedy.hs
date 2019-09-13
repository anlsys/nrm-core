{-|
Module      : Bandit.EpsGreedy
Copyright   : (c) 2019, UChicago Argonne, LLC.
License     : MIT
Maintainer  : fre@freux.fr
-}
module Bandit.EpsGreedy
  ( EpsGreedy (..)
  )
where

import Bandit.Class
import Control.Lens
import Data.Generics.Product
import Data.Random
import qualified Data.Random.Distribution.Categorical as DC
import qualified Data.Random.Sample as RS
import Protolude
import Refined

data EpsGreedy a
  = Initial
      {actions :: [a]}
  | Screening
      { t :: Int
      , lastAction :: Maybe a
      , k :: Int
      , screened :: [Double]
      , actions :: [a]
      }
  | ExploreExploit
      { t :: Int
      , lastAction :: Maybe a
      , k :: Int
      , weights :: [Weight a]
      }

-- | Average loss counter for an action
newtype AverageLoss = AverageLoss {getAverageLoss :: Double}
  deriving (Generic)

-- | EpsGreedy data structure for one action
data Weight a
  = Weight
      { cumulativeLoss :: Maybe (AverageLoss, Int)
      , action :: a
      }
  deriving (Generic)

newtype Epsilon = Epsilon Double

instance (Eq a) => Bandit (EpsGreedy a) Epsilon Set Proxy a Double where

  init _ as _ = Initial {actions = as}

{-step l =-}
pickAction :: (MonadRandom m, MonadState (EpsGreedy a) m) => m a
pickAction = get >>= s >>= btw (assign (field @"lastAction") . Just)
  where
    s bandit = RS.sample . DC.fromWeightedList $ catMaybes $ weights bandit <&> w2tuple
    w2tuple (Weight (Just (l, i)) action) = Just (l / i, action)
    w2tuple (Weight Nothing _) = Nothing

{-updateCumLoss :: Double -> Weight a -> Weight a-}
{-updateCumLoss l w@(Weight (Probability p) (CumulativeLoss cL) _) =-}
{-w & field @"cumulativeLoss" .~ CumulativeLoss (cL + (l / p))-}
btw :: (Functor f) => (t -> f b) -> t -> f t
btw k x = x <$ k x
