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
    evalNum,
    eval,
    evalRange,
  )
where

import CPD.Core as CPD
import CPD.Values
import LMap.Map hiding (singleton)
import Numeric.Interval as I hiding (elem)
import Protolude hiding (Map)

data MeasurementValidation = AdjustInterval (Interval Double) | MeasurementOk

data ActionValidation e = ActionOk | UnknownActuator | InvalidAction

-- | validates a single sensor's range
validateMeasurement :: Interval Double -> Double -> MeasurementValidation
validateMeasurement i x
  | x `member` i = MeasurementOk
  | x < inf i = AdjustInterval $ 2 * x - sup i ... sup i
  | otherwise = AdjustInterval $ inf i ... 2 * x - inf i

validateAction :: Problem -> Action -> ActionValidation Text
validateAction p action = lookup (CPD.Values.actuatorID action) (actuators p) & \case
  Nothing -> UnknownActuator
  Just actuator ->
    if (CPD.Values.actuatorValue action) `elem` CPD.actions actuator
      then ActionOk
      else InvalidAction

-- | Standard object evaluation on Num instances.
evalNum ::
  (Fractional a) =>
  (Double -> a) ->
  Map SensorID a ->
  OExpr ->
  Maybe a
evalNum scalarLifter m = \case
  OValue sensorID -> lookup sensorID m
  OScalar s -> Just (scalarLifter s)
  OAdd a b -> ev2 a b (+)
  OSub a b -> ev2 a b (-)
  OMul a b -> ev2 a b (*)
  ODiv a b -> ev2 a b (/)
  where
    ev = evalNum scalarLifter m
    ev2 a b f = do
      v1 <- ev a
      v2 <- ev b
      return $ f v1 v2

-- | Objective value evaluation.
eval ::
  Map SensorID Double ->
  OExpr ->
  Maybe Double
eval = evalNum (identity :: Double -> Double)

-- | Objective range evaluation.
evalRange ::
  Map SensorID (Interval Double) ->
  OExpr ->
  Maybe (Interval Double)
evalRange = evalNum (singleton :: Double -> Interval Double)
