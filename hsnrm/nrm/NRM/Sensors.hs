{-# LANGUAGE DerivingVia #-}

{-# OPTIONS_GHC -fno-warn-orphans #-}
{-|
Module      : NRM.Sensors
Copyright   : (c) 2019, UChicago Argonne, LLC.
License     : BSD3
Maintainer  : fre@freux.fr
-}
module NRM.Sensors
  ( cpdSensors
  , NRM.Sensors.process
  , NRM.Sensors.perform
  , Output (..)
  , OutputP (..)
  )
where

import CPD.Core as CPD
import CPD.Utils as CPD
import CPD.Values as CPD
import Control.Lens
import Data.Map as DM
import LensMap.Core
import NRM.Classes.Sensors
import NRM.Types.Configuration
import NRM.Types.Sensor
import NRM.Types.State
import NRM.Types.Units
import Protolude

cpdSensors :: NRMState -> Map SensorID CPD.Sensor
cpdSensors st =
  DM.fromList . mconcat $
    [ DM.toList
        (lenses st :: LensMap NRMState ActiveSensorKey ActiveSensor) <&> \(k, ScopedLens sl) -> toCPDSensor (k, view sl st)
    , DM.toList
      (lenses st :: LensMap NRMState PassiveSensorKey PassiveSensor) <&> \(k, ScopedLens sl) -> toCPDSensor (k, view sl st)
    ]

data Output = Adjusted NRMState | Ok NRMState Measurement | NotFound

process
  :: Cfg
  -> Time
  -> NRMState
  -> ActiveSensorKey
  -> Double
  -> Output
process _cfg time st sensorKey value =
  DM.lookup sensorKey (lenses st :: LensMap NRMState ActiveSensorKey ActiveSensor) & \case
    Nothing -> NotFound
    Just (ScopedLens sl) ->
      view sl st & \s ->
        CPD.validateMeasurement
          (range . sensorMeta . snd $ toCPDSensor (sensorKey, s))
          value & \case
          MeasurementOk ->
            Ok st
              ( Measurement
                { sensorID = toS sensorKey
                , sensorValue = value
                , time = time
                }
              )
          AdjustInterval _r -> Adjusted (st & sl %~ (\sensor -> sensor)) -- TODO adjustment

data OutputP = AdjustedP NRMState | OkP NRMState Measurement

perform
  :: Cfg
  -> Time
  -> NRMState
  -> ScopedLens NRMState PassiveSensor
  -> IO Output
perform _cfg time st sensorKey = undefined
