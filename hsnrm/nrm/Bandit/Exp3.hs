{-# LANGUAGE DerivingVia #-}

{-|
Module      : Bandit.Exp3
Copyright   : (c) 2019, UChicago Argonne, LLC.
License     : MIT
Maintainer  : fre@freux.fr
-}
module Bandit.Exp3
  ( Exp3 (..)
  , Weight (..)
  )
where

import Bandit.Class
import Control.Lens
import qualified Data.Aeson as A
import Data.Data
import Data.Generics.Product
import Data.JSON.Schema
import Data.MessagePack
import Data.Random
import qualified Data.Random.Distribution.Categorical as DC
import qualified Data.Random.Sample as RS
import Dhall hiding (field)
import NRM.Classes.Messaging
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
  deriving (JSONSchema, A.ToJSON, A.FromJSON) via GenericJSON Probability
  deriving (Show, Generic, Data, MessagePack, Interpret, Inject)

newtype CumulativeLoss = CumulativeLoss {getCumulativeLoss :: Double}
  deriving (JSONSchema, A.ToJSON, A.FromJSON) via GenericJSON CumulativeLoss
  deriving (Show, Generic, Data, MessagePack, Interpret, Inject)

data Weight a
  = Weight
      { probability :: Probability
      , cumulativeLoss :: CumulativeLoss
      , action :: a
      }
  deriving (Generic)

instance (Eq a) => Bandit (Exp3 a) Set a (Refined (FromTo 0 1) Double) where

  init as = Exp3
    { t = 1
    , lastAction = Nothing
    , k = length as
    , ws = toList as <&> Weight (Probability 1) (CumulativeLoss 0)
    }

  step (unrefine -> l) =
    get <&> lastAction >>= \case
      Nothing -> pickAction
      Just oldAction -> do
        field @"ws" %=
          fmap (\w -> if action w == oldAction then updateCumLoss l w else w)
        t <- use $ field @"t"
        k <- use $ field @"k"
        field @"ws" %= recompute t k
        field @"t" += 1
        pickAction

pickAction :: (MonadRandom m, MonadState (Exp3 a) m) => m a
pickAction = get >>= s >>= btw (assign (field @"lastAction") . Just)
  where
    s bandit = RS.sample . DC.fromWeightedList $ ws bandit <&> w2tuple
    w2tuple (Weight p _ action) = (getProbability p, action)

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

btw :: (Functor f) => (t -> f b) -> t -> f t
btw k x = x <$ k x
