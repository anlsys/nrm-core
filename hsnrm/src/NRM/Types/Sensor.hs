{-# LANGUAGE DerivingVia #-}

-- |
--
-- Module      : NRM.Types.Sensor
-- Copyright   : (c) UChicago Argonne, 2019
-- License     : BSD3
-- Maintainer  : swann@anl.gov
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
import Control.Lens
import Data.Generics.Labels ()
import Dhall
import NRM.Classes.Sensors
import NRM.Types.Configuration (SensorBehavior (..), Tag (..))
import NRM.Types.DownstreamCmdID
import NRM.Types.DownstreamThreadID
import NRM.Types.MemBuffer
import NRM.Types.Topology.PackageID
import qualified NRM.Types.Units as U
import Numeric.Interval
import Protolude

data SensorMeta
  = SensorMeta
      { tags :: [Tag],
        range :: Interval Double,
        last :: Maybe (U.Time, Double),
        lastReferenceMeasurements :: MemBuffer,
        cumulative :: SensorBehavior
      }
  deriving (Generic)

class HasMeta a where
  _meta :: Lens' a SensorMeta

data PassiveSensor
  = PassiveSensor
      { perform :: IO (Maybe Double),
        frequency :: U.Frequency,
        passiveMeta :: SensorMeta
      }
  deriving (Generic)

instance HasMeta PassiveSensor where
  _meta = #passiveMeta

data ActiveSensor
  = ActiveSensor
      { maxFrequency :: U.Frequency,
        process :: Double -> Double,
        activeMeta :: SensorMeta
      }
  deriving (Generic)

instance HasMeta ActiveSensor where
  _meta = #activeMeta

data ActiveSensorKey = DownstreamCmdKey DownstreamCmdID | DownstreamThreadKey DownstreamThreadID
  deriving (Ord, Eq, Show)

data PassiveSensorKey = RaplKey PackageID | ExtraPassiveSensorKey Text
  deriving (Ord, Eq, Show)

data SensorKey = AKey ActiveSensorKey | PKey PassiveSensorKey
  deriving (Ord, Eq, Show)

instance StringConv PassiveSensorKey CPD.SensorID where
  strConv _ = CPD.SensorID . show

instance StringConv ActiveSensorKey CPD.SensorID where
  strConv _ = CPD.SensorID . show

instance ToCPDSensor ActiveSensorKey ActiveSensor where
  toCPDSensor (id, ActiveSensor {..}) =
    (toS id, CPD.Sensor (range activeMeta) maxFrequency)

instance ToCPDSensor PassiveSensorKey PassiveSensor where
  toCPDSensor (id, PassiveSensor {..}) =
    (toS id, CPD.Sensor (range passiveMeta) frequency)
