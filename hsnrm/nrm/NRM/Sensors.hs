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
  , Output (..)
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

cpdSensors = undefined

data Output = Adjusted NRMState | Ok NRMState Measurement | NotFound

process
  :: Cfg
  -> Time
  -> NRMState
  -> ActiveSensorKey
  -> Double
  -> Output
process _cfg _time st sensorKey value =
  DM.lookup sensorKey (lenses st :: LensMap NRMState ActiveSensorKey ActiveSensor) & \case
    Nothing -> NotFound
    Just (ScopedLens sl) ->
      view sl st & \case
        Nothing -> NotFound
        Just s@ActiveSensor {..} -> case CPD.validateMeasurement
          (range . sensorMeta . snd $ toCPDSensor (sensorKey, s))
          value of
          MeasurementOk -> Ok st (Measurement {})
          AdjustInterval r -> Adjusted (st & sl %~ fmap (\sensor -> sensor)) -- TODO adjustment
