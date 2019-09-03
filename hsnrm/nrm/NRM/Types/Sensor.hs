{-# LANGUAGE DerivingVia #-}

{-|

Module      : NRM.Types.Sensor
Copyright   : (c) UChicago Argonne, 2019
License     : BSD3
Maintainer  : fre@freux.fr
-}
module NRM.Types.Sensor
  ( PassiveSensor (..)
  , ActiveSensor (..)
  , CPDLSensor (..)
  , HasSensors (..)
  )
where

{-import Data.Aeson-}

{-import NRM.Types.Topology.Package-}
{-import NRM.Types.Sensor-}
import CPD.Core
{-import Data.MessagePack-}
import Protolude

-- External (CPD) classes

-- | Per the CPD model, a sensor has to be owned by a 'source'.
-- This typeclass is a helper for that.
class HasSensors a ownerContext | a -> ownerContext where

  toSensors :: ownerContext -> a -> Map SensorID Sensor

-- | Typeclass for a sensor that can be represented as
-- a CPD sensor.
class CPDLSensor a sensorContext | a -> sensorContext where

  toSensor :: sensorContext -> a -> (SensorID, Sensor)

-- Internal (NRM) classes

class PassiveSensor a where

  maxFrequency :: a -> Double

  validate :: a -> Value

class ActiveSensor a where

  frequency :: a -> Double

  perform :: a -> Value
