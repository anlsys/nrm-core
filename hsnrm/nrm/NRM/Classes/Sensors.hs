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

import Protolude
import qualified CPD.Core as CPD

class IsSensor a where

  toCPDSensor :: CPD.SensorID -> a -> (CPD.SensorID, CPD.Sensor)

-- Internal (NRM) classes
data PackedSensor = forall a. IsSensor a => MkPackedSensor a

packSensor :: IsSensor a => a -> PackedSensor
packSensor = MkPackedSensor

class HasSensors a aCtx | a -> aCtx where

  listSensors :: aCtx -> a -> Map CPD.SensorID PackedSensor

  adjustRange :: CPD.SensorID -> CPD.Range -> a -> a

toCPDPackedSensor :: CPD.SensorID -> PackedSensor -> (CPD.SensorID, CPD.Sensor)
toCPDPackedSensor id (MkPackedSensor s) = toCPDSensor id s
