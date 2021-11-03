{-# LANGUAGE DerivingVia #-}

-- |
-- Module      : CPD.Utils
-- Copyright   : (c) UChicago Argonne, 2019
-- License     : BSD3
-- Maintainer  : swann@anl.gov
module CPD.Utils
  ( validateAction,
    validateMeasurement,
    MeasurementValidation (..),
    ActionValidation (..),
    evalNum,
    evalRange,
  )
where

import CPD.Core as CPD
import CPD.Values
import qualified Data.Map as M
import Numeric.Interval as I hiding (elem)
import Protolude

data MeasurementValidation = AdjustInterval (Interval Double) | MeasurementOk

data ActionValidation e = ActionOk | UnknownActuator | InvalidAction

-- | validates a single sensor's range
validateMeasurement :: Interval Double -> Double -> MeasurementValidation
validateMeasurement i x
  | x `member` i = MeasurementOk
  | x < inf i = AdjustInterval $ 2 * x - sup i ... sup i
  | otherwise = AdjustInterval $ inf i ... 2 * x - inf i

validateAction :: Problem -> Action -> ActionValidation Text
validateAction p action =
  M.lookup (CPD.Values.actuatorID action) (actuators p) & \case
    Nothing -> UnknownActuator
    Just actuator ->
      if CPD.Values.actuatorValue action `elem` CPD.actions actuator
        then ActionOk
        else InvalidAction

-- | Standard object evaluation on Num instances.
evalNum ::
  Map SensorID Double ->
  Map SensorID Double ->
  OExpr ->
  Maybe Double
evalNum m r = \case
  OValue sensorID -> M.lookup sensorID m
  OReference sensorID -> M.lookup sensorID r
  OScalar s -> Just s
  OAdd a b -> ev2 a b (+)
  OSub a b -> ev2 a b (-)
  OMul a b -> ev2 a b (*)
  ODiv a b -> ev2 a b (/)
  OMin a b -> ev2 a b min
  OMax a b -> ev2 a b max
  where
    ev = evalNum m r
    ev2 a b f = f <$> ev a <*> ev b

-- | Range evaluation
evalRange ::
  Map SensorID (Interval Double) ->
  OExpr ->
  Maybe (Interval Double)
evalRange m = \case
  OValue sensorID -> M.lookup sensorID m
  OReference sensorID -> M.lookup sensorID m
  OScalar s -> Just (singleton s)
  OAdd a b -> ev2 a b (+)
  OSub a b -> ev2 a b (-)
  OMul a b -> ev2 a b (*)
  ODiv a b -> ev2 a b (/)
  OMin a b -> ev2 a b minI
  OMax a b -> ev2 a b maxI
  where
    ev = evalRange m
    ev2 a b f = f <$> ev a <*> ev b
    maxI :: (Ord a) => Interval a -> Interval a -> Interval a
    maxI i i' = max (inf i) (inf i') ... max (sup i) (sup i')
    minI :: (Ord a) => Interval a -> Interval a -> Interval a
    minI i i' = min (inf i) (inf i') ... min (sup i) (sup i')
