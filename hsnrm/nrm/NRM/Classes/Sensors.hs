{-|
Module      : NRM.Classes.Sensors
Copyright   : (c) UChicago Argonne, 2019
License     : BSD3
Maintainer  : fre@freux.fr
-}
module NRM.Classes.Sensors
  ( IsSensor (..)
  , HasSensors (..)
  , PackedSensor
  , packSensor
  , toCPDPackedSensor
  )
where

import qualified CPD.Core as CPD
import NRM.Types.Sensor 
import Protolude

class IsSensor a where

  toCPDSensor :: CPD.SensorID -> a -> (CPD.SensorID, CPD.Sensor)

  toNRMSensor :: a -> Sensor

-- Internal (NRM) classes
data PackedSensor = forall a. IsSensor a => MkPackedSensor a

packSensor :: IsSensor a => a -> PackedSensor
packSensor = MkPackedSensor

class HasSensors a aCtx | a -> aCtx where

  listSensors :: aCtx -> a -> Map CPD.SensorID PackedSensor

  adjustRange :: CPD.SensorID -> CPD.Interval -> a -> a

toCPDPackedSensor :: CPD.SensorID -> PackedSensor -> (CPD.SensorID, CPD.Sensor)
toCPDPackedSensor id (MkPackedSensor s) = toCPDSensor id s

instance IsSensor Sensor where

  toNRMSensor x = x

  toCPDSensor id PassiveSensor {..} =
    ( id
    , CPD.Sensor
      { sensorMeta = CPD.Metadata (uncurry CPD.Interval range)
          (CPD.MaxFrequency frequency)
      , ..
      }
    )
  toCPDSensor id ActiveSensor {..} =
    ( id
    , CPD.Sensor
      { sensorMeta = CPD.Metadata (uncurry CPD.Interval range)
          (CPD.MaxFrequency maxFrequency)
      , ..
      }
    )
