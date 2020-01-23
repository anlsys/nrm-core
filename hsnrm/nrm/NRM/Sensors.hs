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
    NRM.Sensors.process,
    MeasurementOutput (..),
    processPassiveSensor,
    ProcessPassiveSensorOutput (..),
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
import NRM.Types.Configuration
import NRM.Types.Sensor as S
import NRM.Types.State
import NRM.Types.Units
import Numeric.Interval
import Protolude hiding (Map)

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

data MeasurementOutput = Adjusted NRMState | Ok NRMState Measurement | NotFound

process ::
  Cfg ->
  Time ->
  NRMState ->
  ActiveSensorKey ->
  Double ->
  MeasurementOutput
process _cfg time st sensorKey value =
  DM.lookup sensorKey (lenses st :: LensMap NRMState ActiveSensorKey ActiveSensor) & \case
    Nothing -> NotFound
    Just (ScopedLens sl) ->
      view sl st & \s ->
        CPD.validateMeasurement
          (CPD.range . snd $ toCPDSensor (sensorKey, s))
          value
          & \case
            MeasurementOk ->
              Ok
                st
                ( Measurement
                    { sensorID = toS sensorKey,
                      sensorValue = value,
                      time = time
                    }
                )
            AdjustInterval r -> Adjusted (st & sl . field @"activeMeta" . field @"range" .~ r)

data ProcessPassiveSensorOutput
  = IllegalValueRemediation PassiveSensor
  | LegalMeasurement PassiveSensor Measurement

processPassiveSensor ::
  PassiveSensor ->
  Time ->
  SensorID ->
  Double ->
  ProcessPassiveSensorOutput
processPassiveSensor ps@(S.range . meta -> i) time sensorID value
  | value < inf i =
    IllegalValueRemediation $
      ps {last = Nothing, passiveMeta = (meta ps) {S.range = 2 * value - sup i ... sup i}}
  | sup i < value =
    IllegalValueRemediation $
      ps {last = Nothing, passiveMeta = (meta ps) {S.range = inf i ... 2 * value - inf i}}
  | otherwise =
    LegalMeasurement
      (ps & field @"last" ?~ (time, value))
      (Measurement sensorID value time)

data ProcessPassiveSensorFailureOutput
  = LegalFailure
  | IllegalFailureRemediation PassiveSensor

processPassiveSensorFailure ::
  PassiveSensor ->
  Time ->
  ProcessPassiveSensorFailureOutput
processPassiveSensorFailure ps time =
  last ps & \case
    Nothing -> LegalFailure
    Just (oldTime, _)
      | observedFrequency < fromHz (S.frequency ps) ->
        IllegalFailureRemediation ps {S.frequency = observedFrequency & hz}
      | otherwise -> LegalFailure
      where
        observedFrequency = 1 / (fromSeconds time - fromSeconds oldTime)
