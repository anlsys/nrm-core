{-# LANGUAGE RankNTypes #-}

{-|
Module      : NRM.Classes.Sensors
Copyright   : (c) UChicago Argonne, 2019
License     : BSD3
Maintainer  : fre@freux.fr
-}
module NRM.Classes.Sensors
  ( ToCPDSensor (..)
  , Sensors (..)
  , AdjustSensors (..)
  , NoSensors (..)
  , PassiveSensorKey (..)
  , ActiveSensorKey (..)
  , SensorKey (..)
  )
where

import qualified CPD.Core as CPD
import Control.Lens
import Data.Generics.Product
import LensMap.Core
import NRM.Types.LMap as LM
import NRM.Types.Sensor
import NRM.Types.Topology.PackageID
import Protolude

class ToCPDSensor k a where

  toCPDSensor :: (k, a) -> (CPD.SensorID, CPD.Sensor)

instance ToCPDSensor ActiveSensor where

  toCPDSensor (id, ActiveSensor {..}) =
    ( cpdID id
    , CPD.Sensor
      { sensorMeta = CPD.Metadata (uncurry CPD.Interval activeRange)
          (CPD.MaxFrequency maxFrequency)
      , source = activeSource
      , ..
      }
    )

instance ToCPDSensor PassiveSensor where

  toCPDSensor (id, PassiveSensor {..}) =
    ( cpdID id
    , CPD.Sensor
      { sensorMeta = CPD.Metadata (uncurry CPD.Interval passiveRange)
          (CPD.MaxFrequency frequency)
      , source = passiveSource
      , ..
      }
    )

-- Structural
data ActiveSensorKey = DowstreamCmdSensor Text | Misc
  deriving (Ord, Eq)

data PassiveSensorKey = RaplKey PackageID | Misc'
  deriving (Ord, Eq)

data SensorKey = AKey ActiveSensorKey | PKey PassiveSensorKey
  deriving (Ord, Eq)

class ToCPDKey k where

  toKey :: k -> CPD.SensorID

instance ToCPDKey ActiveSensorKey

instance ToCPDKey PassiveSensorKey
