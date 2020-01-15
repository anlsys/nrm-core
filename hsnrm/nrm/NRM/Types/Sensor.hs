{-# LANGUAGE ExistentialQuantification #-}

-- |
--
-- Module      : NRM.Types.Sensor
-- Copyright   : (c) UChicago Argonne, 2019
-- License     : BSD3
-- Maintainer  : fre@freux.fr
module NRM.Types.Sensor
  ( -- * Internal representation
    Tag (..),
    ActiveSensor (..),
    PassiveSensor (..),
    SensorMeta (..),
    HasMeta (..),

    -- * Keys for LensMap
    ActiveSensorKey (..),
    PassiveSensorKey (..),
    SensorKey (..),

    -- * Re-exports
    CPD.SensorID (..),
  )
where

import qualified CPD.Core as CPD
import NRM.Classes.Sensors
import NRM.Types.DownstreamCmdID
import NRM.Types.DownstreamThreadID
import NRM.Types.MemBuffer
import NRM.Types.Topology.PackageID
import qualified NRM.Types.Units as U
import Numeric.Interval
import Protolude

-- | Sensor tags - used to build objectives and constraints.
data Tag
  = Power
  | Rapl
  | DownstreamThreadSignal
  | DownstreamCmdSignal
  | Minimize
  | Maximize
  deriving (Eq, Show)

data PassiveSensor
  = PassiveSensor
      { perform :: IO (Maybe Double),
        frequency :: U.Frequency,
        last :: Maybe (U.Time, Double),
        passiveMeta :: SensorMeta
      }
  deriving (Generic)

data ActiveSensor
  = ActiveSensor
      { maxFrequency :: U.Frequency,
        process :: Double -> Double,
        activeMeta :: SensorMeta
      }
  deriving (Generic)

class HasMeta a where
  meta :: a -> SensorMeta

instance HasMeta ActiveSensor where
  meta = activeMeta

instance HasMeta PassiveSensor where
  meta = passiveMeta

data SensorMeta
  = SensorMeta
      { tags :: [Tag],
        range :: Interval Double,
        lastReferenceMeasurements :: MemBuffer Double
      }
  deriving (Generic)

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
        (range activeMeta)
        maxFrequency
    )

instance ToCPDSensor PassiveSensorKey PassiveSensor where
  toCPDSensor (id, PassiveSensor {..}) =
    ( toS id,
      CPD.Sensor
        (range passiveMeta)
        frequency
    )
