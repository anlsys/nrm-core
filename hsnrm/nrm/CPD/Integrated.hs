{-# LANGUAGE DerivingVia #-}
{-# OPTIONS_GHC -fno-warn-partial-fields #-}

-- |
-- Module      : CPD.Integrated
-- Copyright   : (c) UChicago Argonne, 2019
-- License     : BSD3
-- Maintainer  : fre@freux.fr
module CPD.Integrated
  ( Integrator (..),
    IntegratedProblem (..),
    IntegratorAction (..),
    integrateProblem,
    initIntegrator,
    measureValue,
    squeeze,
  )
where

--calculate,
--Calculate (..),

import CPD.Core
import CPD.Utils
import qualified Data.Aeson as A
import Data.Data
import Data.JSON.Schema
import qualified Data.Map as DM
import Data.MessagePack
import Dhall (Inject, Interpret)
import NRM.Classes.Messaging
import NRM.Types.Units
import Numeric.Interval as I hiding (elem)
import Protolude

data IntegratedProblem
  = IntegratedProblem
      { isensors :: Map SensorID (Interval Double),
        iactuators :: Map ActuatorID Actuator,
        iobjective :: Objective,
        iobjRange :: I.Interval Double
      }
  deriving (Show, Generic, Data, MessagePack, Interpret, Inject)
  deriving
    (JSONSchema, A.ToJSON, A.FromJSON)
    via GenericJSON IntegratedProblem

data Integrator
  = Integrator
      { tLast :: Time,
        minimumControlInterval :: Time,
        measured :: Map SensorID MeasurementState
      }
  deriving (Generic)

trapezoidArea :: (Time, Double) -> (Time, Double) -> Double
trapezoidArea (t1, v1) (t2, v2) =
  min v1 v2 * fromuS (t2 - t1)
    + abs (v2 - v1) / (2 * fromuS (t2 - t1))

measureValue :: Time -> (Time, Double) -> MeasurementState -> MeasurementState
measureValue thresholdTime (newTime, newValue) Never
  | thresholdTime <= newTime = Done newValue newTime newValue
  | otherwise = Running newTime newTime newValue newValue
measureValue _ (newTime, newValue) (Done totalAvg _lastTime _lastValue) =
  Done totalAvg newTime newValue
measureValue thresholdTime (newTime, newValue) (Running firstT lastT lastV avg)
  | newTime < thresholdTime = Running
    { firstTime = firstT,
      lastTime = newTime,
      lastValue = newValue,
      average =
        ( trapezoidArea (lastT, lastV) (newTime, newValue)
            + avg * fromuS (lastT - firstT)
        )
          / fromuS (newTime - firstT)
    }
  | otherwise = Done
    { lastTimeDone = newTime,
      lastValueDone = newValue,
      totalAverageDone =
        trapezoidArea (lastT, lastV) (newTime, newValue)
          * fromuS ((newTime - thresholdTime) / (newTime - lastT))
    }

squeeze ::
  Time ->
  Map SensorID MeasurementState ->
  Maybe (Map SensorID Double, Map SensorID MeasurementState)
squeeze _t mstM =
  if all isDone (DM.elems mstM)
    then Just (mstM <&> totalAverageDone, newMeasurements)
    else Nothing
  where
    newMeasurements = mstM <&> newround
    newround (Done _totalAverageDone lastTimeDone lastValueDone) =
      Running lastTimeDone lastTimeDone lastValueDone lastValueDone
    newround _ = Never
    isDone Done{} = True
    isDone _ = False

data MeasurementState
  = Never
  | Running
      { firstTime :: Time,
        lastTime :: Time,
        lastValue :: Double,
        average :: Double
      }
  | Done
      { totalAverageDone :: Double,
        lastTimeDone :: Time,
        lastValueDone :: Double
      }
  deriving (Show, Data, MessagePack, Generic, Inject, Interpret)
  deriving
    (JSONSchema, A.ToJSON, A.FromJSON)
    via GenericJSON MeasurementState

data IntegratorAction = IntegratorPasses | TriggerStep Integrator

integrateProblem :: Problem -> Maybe IntegratedProblem
integrateProblem p =
  IntegratedProblem
    <$> Just ranges
    <*> Just (actuators p)
    <*> Just (objective p)
    <*> (evalRange ranges =<< objective p)
  where
    ranges = sensors p <&> range

initIntegrator ::
  Time ->
  Time ->
  Integrator
initIntegrator t tmin = Integrator
  { tLast = t,
    minimumControlInterval = tmin,
    measured = DM.empty
  }
-- calculate ::
--   Time ->
--   Integrator ->
--   IntegratedProblem ->
--   Maybe Calculate
-- calculate
--   t
--   ( clear t ->
--       ( Integrator
--           tLast
--           deltaT
--           timeSeriesMap
--         )
--     )
--   ( IntegratedProblem
--       sensors
--       actuators
--       objective
--       range
--     )
--     | t - tLast < deltaT = Nothing
--     | otherwise =
--       let integrated :: [Maybe (SensorID, ProcessedTS)]
--           integrated =
--             ( DM.toList
--                 timeSeriesMap
--                 <&> (\(sensorID, ts) -> (sensorID,) <$> integrate ts)
--             )
--           maybeResults :: Maybe [(SensorID, ProcessedTS)]
--           maybeResults = sequence integrated
--        in ptsToCalculate <$> maybeResults
--     where
--       integrate :: [(Time, Double)] -> Maybe ProcessedTS
--       integrate ((tFirst, vFirst) : (tSecond, vSecond) : ts)
--         | fst (last ts) < tLast + deltaT = Nothing
--         | otherwise =
--           let proxyValue =
--                 vSecond + fromuS (tSecond - tLast)
--                   * ((vFirst - vSecond) / fromuS (tFirst - tSecond))
--            in Just $
--                 ProcessedTS
--                   [p | p@(tp, _) <- ts, tp >= t]
--                   ( snd $
--                       foldl
--                         folder
--                         ((tLast, proxyValue), 0)
--                         ( (tSecond, vSecond)
--                             : ts
--                         )
--                   )
--       integrate _ = Nothing
--       folder ::
--         ((Time, Double), Double) ->
--         (Time, Double) ->
--         ((Time, Double), Double)
--       folder ((t', previous), accum) (t'', value)
--         | t'' < t + deltaT = ((t'',value),(t'' - t') * min value previous)
--         | otherwise = (t', accum)
