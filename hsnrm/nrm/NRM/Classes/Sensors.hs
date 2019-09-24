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
  )
where

import qualified CPD.Core as CPD
import NRM.Types.LMap as LM
import NRM.Types.Sensor
import Protolude

class ToCPDSensor a where

  toCPDSensor :: (CPD.SensorID, a) -> (CPD.SensorID, CPD.Sensor)

instance ToCPDSensor ActiveSensor where

  toCPDSensor (id, ActiveSensor {..}) =
    ( id
    , CPD.Sensor
      { sensorMeta = CPD.Metadata (uncurry CPD.Interval activeRange)
          (CPD.MaxFrequency maxFrequency)
      , source = activeSource
      , ..
      }
    )

instance ToCPDSensor PassiveSensor where

  toCPDSensor (id, PassiveSensor {..}) =
    ( id
    , CPD.Sensor
      { sensorMeta = CPD.Metadata (uncurry CPD.Interval passiveRange)
          (CPD.MaxFrequency frequency)
      , source = passiveSource
      , ..
      }
    )

-- Structural
class Sensors a where

  activeSensors :: a -> LMap CPD.SensorID ActiveSensor

  passiveSensors :: a -> LMap CPD.SensorID PassiveSensor

class AdjustSensors a where

  adjust :: CPD.SensorID -> CPD.Interval -> a -> a

-- Newtype adapter for DerivingVia
instance (AdjustSensors (k, v)) => AdjustSensors (LMap k v) where

  adjust i r = LM.mapKV (adjust i r)

newtype NoSensors (a :: Type) = NoSensors {unNoSensors :: a}

instance Sensors (NoSensors a) where

  passiveSensors = const LM.empty

  activeSensors = const LM.empty

instance AdjustSensors (NoSensors a) where

  adjust _ _ x = x

-- Sensor querying
instance (Sensors (k, v)) => Sensors (LMap k v) where

  activeSensors (LM.toList -> m) = mconcat (m <&> activeSensors)

  passiveSensors (LM.toList -> m) = mconcat (m <&> passiveSensors)

instance (Sensors a) => Sensors (Maybe a) where

  activeSensors (Just x) = activeSensors x
  activeSensors Nothing = LM.empty

  passiveSensors (Just x) = passiveSensors x
  passiveSensors Nothing = LM.empty

instance (AdjustSensors a) => AdjustSensors (Maybe a) where

  adjust id interval x = adjust id interval <$> x
