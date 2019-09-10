{-|
Module      : Bandit.Exp3
Copyright   : (c) 2019, UChicago Argonne, LLC.
License     : MIT
Maintainer  : fre@freux.fr
-}
module Bandit.Exp3
  ( Exp3
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

data Exp3 a
  = Exp3
      { t :: Int
      , lastAction :: Maybe a
      , k :: Int
      , ws :: [Weight a]
      }
  deriving (Generic)

newtype Probability = Probability {getProbability :: Double}
  deriving (Generic)

newtype CumulativeLoss = CumulativeLoss {getCumulativeLoss :: Double}
  deriving (Generic)

data Weight a
  = Weight
      { probability :: Probability
      , cumulativeLoss :: CumulativeLoss
      , action :: a
      }
  deriving (Generic)

sampleExp3 :: MonadRandom m => Exp3 a -> m a
sampleExp3 bandit =
  RS.sample . DC.fromWeightedList $
    ws bandit <&> \(Weight p _ aktion) -> (getProbability p, aktion)

instance (Eq a) => Bandit (Exp3 a) Set a (Refined (FromTo 0 1) Double) where

  getAction = lastAction

  init as = Exp3
    { t = 1
    , lastAction = Nothing
    , k = length as
    , ws = toList as <&> Weight (Probability 1) (CumulativeLoss 0)
    }

  step b (unrefine -> l) = case lastAction b of
    Nothing -> sampleExp3 b <&> \firstAction -> b & field @"lastAction" ?~ firstAction
    Just i -> evalStateT (exp3 i) b
    where
      exp3 i = do
        field @"ws" %=
          fmap (\w -> if action w == i then updateCumLoss l w else w)
        t <- use $ field @"t"
        k <- use $ field @"k"
        field @"ws" %= recompute t k
        field @"t" += 1
        newAction <- get >>= lift . sampleExp3
        field @"lastAction" ?= newAction
        get

updateCumLoss :: Double -> Weight a -> Weight a
updateCumLoss l w@(Weight (Probability p) (CumulativeLoss cL) _) =
  w & field @"cumulativeLoss" .~ CumulativeLoss (cL + (l / p))

recompute :: Int -> Int -> [Weight a] -> [Weight a]
recompute t k ws = updatep <$> ws
  where
    updatep w@(Weight _ (CumulativeLoss cL) _) =
      w & field @"probability" . field @"getProbability" .~
        expw cL /
        denom
    expw cL =
      exp (- sqrt (2.0 * log (fromIntegral k) / fromIntegral (t * k)) * cL)
    denom = getSum $ foldMap denomF ws
    denomF (getCumulativeLoss . cumulativeLoss -> cL) = Sum $ expw cL
