{-# LANGUAGE DerivingVia #-}

-- |
-- Module      : CPD.Utils
-- Copyright   : (c) UChicago Argonne, 2019
-- License     : BSD3
-- Maintainer  : fre@freux.fr
module CPD.Utils
  ( validateAction,
    validateMeasurement,
    MeasurementValidation (..),
    ActionValidation (..),
    combine,
    eval,
  )
where

import CPD.Core as CPD
import CPD.Values as CPD
import qualified Data.Map as DM
import Numeric.Interval as I hiding (elem)
import Protolude

data MeasurementValidation = AdjustInterval (Interval Double) | MeasurementOk

data ActionValidation = ActionOk | InvalidAction

-- | validates a single sensor's range
validateMeasurement :: Interval Double -> Double -> MeasurementValidation
validateMeasurement i x
  | x `member` i = MeasurementOk
  | x <= inf i = AdjustInterval $ (inf i - width i) ... sup i
  | otherwise = AdjustInterval $ inf i ... (sup i + width i)

validateAction :: Admissible -> Action -> ActionValidation
validateAction (Admissible ds) (Action _actuator d)
  | d `elem` ds = ActionOk
  | otherwise = InvalidAction

-- | Combines problems by adding sensor lists and actuator lists
combine :: Problem -> Problem -> Maybe Problem
combine (Problem a b goal) (Problem c d goal')
  | goal == goal' = Just $ Problem (a <> c) (b <> d) goal
  | otherwise = Nothing

{-availableDiscreteActions :: Problem -> [Actions]-}
{-availableDiscreteActions (Problem _ as _) = mconcat $ as <&> \aKv ->
  [Actions (aKv ^. field @"actuatorKey") a | a <- listDiscreteActuatorActions]-}
data V = V Double (I.Interval Double)

eval :: Map SensorID V -> OExpr -> Maybe V
eval m = \case
  OValue sensorID -> DM.lookup sensorID m
  OScalarMult s a -> ev a <&> (\(V x i) -> V (s * x) $ singleton s * i)
  OAdd a b -> ev2 a b $ \(V x1 i1) (V x2 i2) -> V (x1 + x2) (i1 + i2)
  OSub a b -> ev2 a b $ \(V x1 i1) (V x2 i2) -> V (x1 - x2) (i1 - i2)
  OMul a b -> ev2 a b $ \(V x1 i1) (V x2 i2) -> V (x1 * x2) (i1 * i2)
  ODiv a b -> ev2 a b $ \(V x1 i1) (V x2 i2) -> V (x1 / x2) (i1 / i2)
  where
    ev = eval m
    ev2 a b f = do
      v1 <- ev a
      v2 <- ev b
      return $ f v1 v2
