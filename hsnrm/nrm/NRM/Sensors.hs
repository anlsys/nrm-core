{-# LANGUAGE DerivingVia #-}

{-# OPTIONS_GHC -fno-warn-orphans #-}
{-|
Module      : NRM.Sensors
Copyright   : (c) 2019, UChicago Argonne, LLC.
License     : BSD3
Maintainer  : fre@freux.fr
-}
module NRM.Sensors
  ( adjustSensorRange
  , cpdSensors
  )
where

{-, cpdSensors-}
import CPD.Core as CPD
import Control.Lens
import Data.Generics.Product
import NRM.Classes.Sensors
import NRM.Types.LMap as LM
import NRM.Types.State
import Protolude

adjustSensorRange
  :: SensorID
  -> Interval
  -> NRMState
  -> Identity NRMState
adjustSensorRange sensorID range =
  constraints'
    @AdjustSensors
    (pure . adjust sensorID range)

cpdSensors :: NRMState -> LMap CPD.SensorID CPD.Sensor
cpdSensors s = (passiveSensors s) & LM.mapKV toCPDSensor

-- Recursive sensor instances
-- Leaf NoSensor instances
deriving via (NoSensors (CPD.Problem)) instance AdjustSensors CPD.Problem

deriving via (NoSensors (CPD.Problem)) instance Sensors CPD.Problem
