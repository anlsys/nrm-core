{-|
Module      : Bandit.Exp3
Copyright   : (c) 2019, UChicago Argonne, LLC.
License     : MIT
Maintainer  : fre@freux.fr
-}
module Bandit.Exp3
  ( Exp3 (..)
  )
where

import Bandit.Class
import Control.Lens
import Data.Generics.Product
import qualified Data.Random.Distribution.Categorical as DC
import qualified Data.Random.Sample as RS
import Data.Random
import Protolude
import Refined

data Exp3 f
  = Exp3
      { t :: Int
      , a :: Maybe Int
      , k :: Int
      , ws :: f Weight
      }
  deriving (Generic)

newtype Probability = Probability {getProbability :: Double}
  deriving (Generic)

newtype CumulativeLoss = CumulativeLoss {getCumulativeLoss :: Double}
  deriving (Generic)

data Weight
  = Weight
      { probability :: Probability
      , cumulativeLoss :: CumulativeLoss
      }
  deriving (Generic)

toCategorical :: f Weight -> DC.Categorical Double Int
toCategorical = undefined

sampleExp3 :: MonadRandom m => Exp3 f -> m Int
sampleExp3 bandit = RS.sample (toCategorical $ ws bandit)

instance
  ( Functor f
  , Traversable f
  , Index (f Weight) ~ Int
  , IxValue (f Weight) ~ Weight
  , Ixed (f Weight)
  )
  => Bandit (Exp3 f) f Int (Refined (FromTo 0 1) Double) where

  action Exp3 {..} = undefined

  init as = Exp3
    { t = 1
    , a = Nothing
    , k = length as
    , ws = Weight (Probability 1) (CumulativeLoss 0) <$ as
    }

  step b (unrefine -> l) = case a b of
    Nothing -> sampleExp3 b <&> Just <&> \a -> Exp3 {..}
    Just i -> do
      let b' =
            b &
              (field @"ws" . ix i %~ updateCumLoss) &
              (field @"ws" %~ recompute) &
              (field @"t" +~ 1)
      i <- sampleExp3 b'
      return $ b' & field @"a" ?~ i
    where
      updateCumLoss w@(Weight (Probability p) (CumulativeLoss cL)) =
        w & field @"cumulativeLoss" .~ CumulativeLoss (cL + (l / p))
      recompute ws = updatep <$> ws
        where
          updatep w@(Weight _ (CumulativeLoss cL)) =
            w & field @"probability" . field @"getProbability" .~
              expw cL /
              denominator
          expw l =
            exp (- sqrt (2.0 * log (fromIntegral (k b)) / fromIntegral (t b * k b)) * l)
          denominator =
            getSum $
              foldMap
                ( \(getCumulativeLoss . cumulativeLoss -> cL) ->
                  Sum $ expw cL
                )
                ws
