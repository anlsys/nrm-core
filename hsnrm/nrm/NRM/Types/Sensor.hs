{-# LANGUAGE ExistentialQuantification #-}

{-|

Module      : NRM.Types.Sensor
Copyright   : (c) UChicago Argonne, 2019
License     : BSD3
Maintainer  : fre@freux.fr
-}
module NRM.Types.Sensor
  ( -- * Internal representation
    Sensor (..)
  , Tag (..)
  , ActiveSensor (..)
  , PassiveSensor (..)
  , -- * Keys for LensMap
    ActiveSensorKey (..)
  , PassiveSensorKey (..)
  , SensorKey (..)
  , -- * Re-exports
    CPD.Source (..)
  , CPD.SensorID (..)
  )
where

import qualified CPD.Core as CPD
import NRM.Classes.Sensors
import NRM.Types.DownstreamCmdID
import NRM.Types.Topology.PackageID
import qualified NRM.Types.Units as U
import Protolude

newtype Tag = Tag Text

data Sensor
  = Passive PassiveSensor
  | Active ActiveSensor

data PassiveSensor
  = PassiveSensor
      { perform :: IO (Maybe Double)
      , passiveTags :: [Tag]
      , frequency :: U.Frequency
      , passiveSource :: CPD.Source
      , passiveRange :: (Double, Double)
      }
  deriving (Generic)

data ActiveSensor
  = ActiveSensor
      { maxFrequency :: U.Frequency
      , activeTags :: [Tag]
      , process :: Double -> Double
      , activeSource :: CPD.Source
      , activeRange :: (Double, Double)
      }

data ActiveSensorKey = DownstreamCmdKey DownstreamCmdID | Misc
  deriving (Ord, Eq)

data PassiveSensorKey = RaplKey PackageID | Misc'
  deriving (Ord, Eq)

data SensorKey = AKey ActiveSensorKey | PKey PassiveSensorKey
  deriving (Ord, Eq)

instance ToCPDSensor ActiveSensorKey ActiveSensor where

  toCPDSensor (id, ActiveSensor {..}) =
    ( toKey id
    , CPD.Sensor
      { sensorMeta = CPD.Metadata (uncurry CPD.Interval activeRange)
          (CPD.MaxFrequency maxFrequency)
      , source = activeSource
      , ..
      }
    )

instance ToCPDSensor PassiveSensorKey PassiveSensor where

  toCPDSensor (id, PassiveSensor {..}) =
    ( toKey id
    , CPD.Sensor
      { sensorMeta = CPD.Metadata (uncurry CPD.Interval passiveRange)
          (CPD.MaxFrequency frequency)
      , source = passiveSource
      , ..
      }
    )

instance ToCPDKey ActiveSensorKey

instance ToCPDKey PassiveSensorKey
