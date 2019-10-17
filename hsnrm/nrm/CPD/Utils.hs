{-# LANGUAGE DerivingVia #-}

{-|
Module      : CPD.Utils
Copyright   : (c) UChicago Argonne, 2019
License     : BSD3
Maintainer  : fre@freux.fr
-}
module CPD.Utils
  ( validateAction
  , validateMeasurement
  , MeasurementValidation (..)
  , ActionValidation (..)
  , combine
  , requiredSensors
  , evalObjective
  )
where

import CPD.Core
import CPD.Values
import qualified Data.Map as DM
import Protolude

data MeasurementValidation = AdjustInterval Interval | MeasurementOk

data ActionValidation = ActionOk | InvalidAction

-- | validates a single sensor's range
validateMeasurement :: Interval -> Double -> MeasurementValidation
validateMeasurement (Interval a b) d
  | a <= d && d <= b = MeasurementOk
  | d < a = AdjustInterval $ Interval (2 * a - b) b
  | otherwise = AdjustInterval $ Interval a (2 * b - a)

validateAction :: Admissible -> Action -> ActionValidation
validateAction (Admissible ds) (Action _actuator d) =
  if d `elem` ds
  then ActionOk
  else InvalidAction

-- | Combines problems by adding sensor lists and actuator lists
combine :: Problem -> Problem -> Maybe Problem
combine (Problem a b goal) (Problem c d goal')
  | goal == goal' = Just $ Problem (a <> c) (b <> d) goal
  | otherwise = Nothing

-- | computes the list of sensors required for the goal to be computed entirely.
requiredSensors :: Problem -> [SensorID]
requiredSensors = fmap x . linearCombination . objective

{-availableDiscreteActions :: Problem -> [Actions]-}
{-availableDiscreteActions (Problem _ as _) = mconcat $ as <&> \aKv ->
  [Actions (aKv ^. field @"actuatorKey") a | a <- listDiscreteActuatorActions]-}

{-listDiscreteActuatorActions :: Actuator -> Actions-}
{-listDiscreteActuatorActions = undefined-}
evalObjective :: Map SensorID Double -> OExpr -> Maybe Double
evalObjective m = \case
  (OValue sensorID) -> DM.lookup sensorID m
  OScalarMult x a -> fmap (* x) (ev a)
  OAdd a b -> do
    x <- ev a
    y <- ev b
    return $ x + y
  OSub a b -> do
    x <- ev a
    y <- ev b
    return $ x - y
  OMul a b -> do
    x <- ev a
    y <- ev b
    return $ x * y
  ODiv a b -> do
    x <- ev a
    y <- ev b
    return $ x / y
  where
    ev = evalObjective m
