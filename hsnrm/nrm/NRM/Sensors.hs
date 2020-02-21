{-# LANGUAGE DerivingVia #-}
{-# LANGUAGE TypeApplications #-}
{-# OPTIONS_GHC -fno-warn-orphans #-}

-- |
-- Module      : NRM.Sensors
-- Copyright   : (c) 2019, UChicago Argonne, LLC.
-- License     : BSD3
-- Maintainer  : fre@freux.fr
module NRM.Sensors
  ( cpdSensors,
    postProcessSensor,
    --postProcessSensorLens,
    MeasurementContext (..),
    processPassiveSensorFailure,
    ProcessPassiveSensorFailureOutput (..),
  )
where

import CPD.Core as CPD
import CPD.Utils as CPD
import CPD.Values as CPD
import Control.Lens hiding ((...))
import Data.Generics.Product
import LMap.Map as DM
import LensMap.Core
import NRM.Classes.Sensors
import NRM.Types.Sensor as S
import NRM.Types.State
import NRM.Types.Units
import Protolude hiding (Map)

--traverseSensors :: Traversal NRMState (Map SensorID CPD.Sensor) (SensorID,CPD.Sensor) (SensorID,CPD.Sensor)
--traverseSensors = undefined

cpdSensors :: NRMState -> Map SensorID CPD.Sensor
cpdSensors st =
  DM.fromList $
    mconcat
      [ DM.toList
          (lenses st :: LensMap NRMState ActiveSensorKey ActiveSensor)
          <&> \(k, ScopedLens sl) -> toCPDSensor (k, view sl st),
        DM.toList
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

--postProcessSensorLens ::
--(HasMeta sensor, StringConv key CPD.SensorID) =>
--Time ->
--key ->
--Double ->
--Maybe sensor ->
--(MeasurementContext Measurement, Maybe sensor)
--postProcessSensorLens time key mvalue = \case
--Nothing -> (NotFound, Nothing)
--Just sensor -> postProcessSensor time key mvalue sensor & _2 %~ Just

postProcessSensor ::
  (HasMeta sensor, StringConv key CPD.SensorID) =>
  Time ->
  key ->
  Double ->
  sensor ->
  (MeasurementContext Measurement, sensor)
postProcessSensor time sensorKey measurement sensor = maybeValue & \case
  Nothing -> (NoMeasurement, new)
  Just value ->
    CPD.validateMeasurement
      (sensor ^. _meta . field @"range")
      value
      & \case
        (AdjustInterval r) ->
          (Adjusted, new & _meta . field @"range" .~ r)
        MeasurementOk ->
          (Ok (Measurement (toS sensorKey) value time), new)
  where
    maybeValue = mkValue (sensor ^. _meta) measurement
    new = sensor & _meta . field @"last" ?~ (time, measurement)

data ProcessPassiveSensorFailureOutput
  = LegalFailure
  | IllegalFailureRemediation PassiveSensor

processPassiveSensorFailure ::
  PassiveSensor ->
  Time ->
  ProcessPassiveSensorFailureOutput
processPassiveSensorFailure ps time =
  ps ^. _meta . field @"last" & \case
    Nothing -> LegalFailure
    Just (oldTime, _)
      | observedFrequency < fromHz (S.frequency ps) ->
        IllegalFailureRemediation ps {S.frequency = observedFrequency & hz}
      | otherwise -> LegalFailure
      where
        observedFrequency = 1 / (fromSeconds time - fromSeconds oldTime)
