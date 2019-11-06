{-# LANGUAGE DerivingVia #-}
{-# OPTIONS_GHC -fno-warn-partial-fields #-}

-- |
-- Module      : CPD.Integrated
-- Copyright   : (c) UChicago Argonne, 2019
-- License     : BSD3
-- Maintainer  : fre@freux.fr
module CPD.Integrated
  ( Integrator (..),
    IntegratorAction (..),
    initIntegrator,
    measureValue,
    squeeze,
  )
where

--calculate,
--Calculate (..),

import CPD.Core
import qualified Data.Aeson as A
import Data.Data
import Data.JSON.Schema
import qualified Data.Map as DM
import Data.MessagePack
import Dhall (Inject, Interpret)
import NRM.Classes.Messaging
import NRM.Types.Units
import Protolude

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
    isDone Done {} = True
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

initIntegrator ::
  Time ->
  Time ->
  [SensorID] ->
  Integrator
initIntegrator t tmin sensorIDs = Integrator
  { tLast = t,
    minimumControlInterval = tmin,
    measured = DM.fromList (sensorIDs <&> (,Never))
  }
