{-# LANGUAGE DerivingVia #-}

{-|
Module      : CPD.Utils
Copyright   : (c) UChicago Argonne, 2019
License     : BSD3
Maintainer  : fre@freux.fr
-}
module CPD.Utils
  ( validateRange
  , Validation (..)
  , measure
  , ValidatedMeasurement (..)
  , combine
  , requiredSensors
  )
where

import CPD.Core
import CPD.Values
import Data.Map as DM
import Protolude

data Validation = AdjustRange Range | Ok | Illegal

-- | validates a single sensor's range
validateRange :: Range -> Value -> Validation
validateRange (Set ds) (DiscreteValue d) =
  if d `elem` ds
  then Ok
  else Illegal
validateRange (Interval a b) (ContinuousValue d)
  | a <= d && d <= b = Ok
  | d < a = AdjustRange $ Interval (2 * a - b) b
  | otherwise = AdjustRange $ Interval a (2 * b - a)
validateRange _ _ = Illegal

data ValidatedMeasurement
  = Measured Measurement
  | AdjustProblem Problem
  | NoSuchSensor

-- | builds a measurement using a problem as context (with validation)
measure :: Problem -> Double -> SensorID -> ValidatedMeasurement
measure (Problem sl _ _) _ sensorID =
  case DM.lookup sensorID (DM.fromList $ (\(SensorKV x y) -> (x, y)) <$> sl) of
    Nothing -> NoSuchSensor -- TODO
    Just _ -> NoSuchSensor -- TODO

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
