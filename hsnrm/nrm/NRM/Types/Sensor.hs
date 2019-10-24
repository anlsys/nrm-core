{-# LANGUAGE ExistentialQuantification #-}

-- |
--
-- Module      : NRM.Types.Sensor
-- Copyright   : (c) UChicago Argonne, 2019
-- License     : BSD3
-- Maintainer  : fre@freux.fr
module NRM.Types.Sensor
  ( -- * Internal representation
    Sensor (..),
    Tag (..),
    ActiveSensor (..),
    PassiveSensor (..),

    -- * Keys for LensMap
    ActiveSensorKey (..),
    PassiveSensorKey (..),
    SensorKey (..),

    -- * Re-exports
    CPD.Source (..),
    CPD.SensorID (..),
  )
where

import qualified CPD.Core as CPD
import NRM.Classes.Sensors
import NRM.Types.DownstreamCmdID
import NRM.Types.DownstreamThreadID
import NRM.Types.Topology.PackageID
import qualified NRM.Types.Units as U
import Numeric.Interval
import Protolude

newtype Tag = Tag Text

data Sensor
  = Passive PassiveSensor
  | Active ActiveSensor

data PassiveSensor
  = PassiveSensor
      { perform :: IO (Maybe Double),
        passiveTags :: [Tag],
        frequency :: U.Frequency,
        passiveSource :: CPD.Source,
        passiveRange :: Interval Double,
        last :: Maybe (U.Time, Double)
      }
  deriving (Generic)

data ActiveSensor
  = ActiveSensor
      { maxFrequency :: U.Frequency,
        activeTags :: [Tag],
        process :: Double -> Double,
        activeSource :: CPD.Source,
        activeRange :: Interval Double
      }

data ActiveSensorKey = DownstreamCmdKey DownstreamCmdID | DownstreamThreadKey DownstreamThreadID
  deriving (Ord, Eq, Show)

data PassiveSensorKey = RaplKey PackageID | Misc'
  deriving (Ord, Eq, Show)

data SensorKey = AKey ActiveSensorKey | PKey PassiveSensorKey
  deriving (Ord, Eq, Show)

instance StringConv PassiveSensorKey CPD.SensorID where
  strConv _ = CPD.SensorID . show

instance StringConv ActiveSensorKey CPD.SensorID where
  strConv _ = CPD.SensorID . show

instance ToCPDSensor ActiveSensorKey ActiveSensor where
  toCPDSensor (id, ActiveSensor {..}) =
    ( toS id,
      CPD.Sensor
        { sensorMeta =
            CPD.Metadata
              activeRange
              (CPD.MaxFrequency maxFrequency),
          source = activeSource,
          ..
        }
    )

instance ToCPDSensor PassiveSensorKey PassiveSensor where
  toCPDSensor (id, PassiveSensor {..}) =
    ( toS id,
      CPD.Sensor
        { sensorMeta = CPD.Metadata passiveRange (CPD.MaxFrequency frequency),
          source = passiveSource,
          ..
        }
    )
