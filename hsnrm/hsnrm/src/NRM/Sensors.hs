{-# LANGUAGE DerivingVia #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# OPTIONS_GHC -fno-warn-orphans #-}

-- |
-- Module      : NRM.Sensors
-- Copyright   : (c) 2019, UChicago Argonne, LLC.
-- License     : BSD3
-- Maintainer  : fre@freux.fr
module NRM.Sensors
  ( cpdSensors,
    postProcessSensor,
    MeasurementContext (..),
    processPassiveSensorFailure,
    ProcessPassiveSensorFailureOutput (..),
  )
where

import CPD.Core as CPD
import CPD.Utils as CPD
import CPD.Values as CPD
import Control.Lens hiding ((...))
import Data.Generics.Labels ()
import Data.Map as M
import LensMap.Core
import NRM.Classes.Sensors
import NRM.Types.Sensor as S
import NRM.Types.State
import NRM.Types.Units
import Protolude hiding (Map)

cpdSensors :: NRMState -> Map SensorID CPD.Sensor
cpdSensors st =
  M.fromList $
    mconcat
      [ M.toList
          (lenses st :: LensMap NRMState ActiveSensorKey ActiveSensor)
          <&> \(k, ScopedLens sl) -> toCPDSensor (k, view sl st),
        M.toList
          (lenses st :: LensMap NRMState PassiveSensorKey PassiveSensor)
          <&> \(k, ScopedLens sl) -> toCPDSensor (k, view sl st)
      ]

mkValue :: SensorMeta -> Double -> Maybe Double
mkValue metaData mvalue = last metaData <&> \(_, lastValue) -> cumulative metaData & \case
  Cumulative -> mvalue - lastValue
  CumulativeWithCapacity maxCounterValue ->
    if lastValue < mvalue
      then mvalue - lastValue
      else maxCounterValue - lastValue + mvalue
  IntervalBased -> mvalue

data MeasurementContext a = Adjusted | Ok a | NoMeasurement

postProcessSensor ::
  (HasMeta sensor, StringConv key CPD.SensorID) =>
  Time ->
  key ->
  Double ->
  sensor ->
  (MeasurementContext Measurement, sensor)
postProcessSensor time sensorKey measurement sensor =
  (,) <$> maybeValue <*> sensor ^? _meta . #last . _Just . _1 & \case
    Nothing -> (NoMeasurement, new)
    Just ((measured, oldTime) :: (Double, Time)) ->
      let value = measured / fromSeconds (time - oldTime)
       in CPD.validateMeasurement (sensor ^. _meta . #range) value & \case
            (AdjustInterval r) -> (Adjusted, new & _meta . #range .~ r)
            MeasurementOk -> (Ok (Measurement (toS sensorKey) value time), new)
  where
    maybeValue = mkValue (sensor ^. _meta) measurement
    new = sensor & _meta . #last ?~ (time, measurement)

data ProcessPassiveSensorFailureOutput
  = LegalFailure
  | IllegalFailureRemediation PassiveSensor

processPassiveSensorFailure ::
  PassiveSensor ->
  Time ->
  ProcessPassiveSensorFailureOutput
processPassiveSensorFailure ps time =
  ps ^. _meta . #last & \case
    Nothing -> LegalFailure
    Just (oldTime, _)
      | observedFrequency < fromHz (S.frequency ps) ->
        IllegalFailureRemediation ps {S.frequency = observedFrequency & hz}
      | otherwise -> LegalFailure
      where
        observedFrequency = 1 / (fromSeconds time - fromSeconds oldTime)
