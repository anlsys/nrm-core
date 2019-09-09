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
  , Measurement (..)
  , combine
  )
where

import CPD.Core
import CPD.Values
import Data.Map as DM
import Protolude

data Validation = AdjustRange Range | Ok | Illegal

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

data Measurement
  = Measured SensorData
  | AdjustProblem Problem SensorData
  | NoSuchSensor

measure :: Problem -> Double -> SensorID -> Measurement
measure (Problem sl _ _) _ sensorID =
  case DM.lookup sensorID (DM.fromList $ (\(SensorKV x y) -> (x, y)) <$> sl) of
    Nothing -> NoSuchSensor -- TODO
    Just _ -> NoSuchSensor -- TODO

combine :: Problem -> Problem -> Maybe Problem
combine (Problem a b goal) (Problem c d goal')
  | goal == goal' = Just $ Problem (a <> c) (b <> d) goal
  | otherwise = Nothing
