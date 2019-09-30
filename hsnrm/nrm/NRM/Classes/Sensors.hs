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
import NRM.Types.LMap as LM
import NRM.Types.Sensor
import NRM.Types.Topology.PackageID
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
data ActiveSensorKey = DowstreamCmdSensor Text

data PassiveSensorKey = RaplKey PackageID

data SensorKey = AKey ActiveSensorKey | PKey PassiveSensorKey

class Sensors a where

  activeSensors :: a -> LMap ActiveSensorKey ActiveSensor

  passiveSensors :: a -> LMap PassiveSensorKey PassiveSensor

class AdjustSensors a where

  adjust :: SensorKey -> CPD.Interval -> a -> a

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

data MySensor = MySensor

data A = A

data B = B

data C = C

newtype Machin = Machin {getMachin :: LMap Int C}
  deriving (Generic)

data MyState
  = MyState
      { truc :: LMap Int A
      , chose :: LMap Int B
      , machin :: LMap Int Machin
      }
  deriving (Generic)

newtype ScopedLens s a
  = ScopedLens {getScopedLens :: Lens' s a}

addPath :: Lens'  s' s -> ScopedLens s a -> ScopedLens  s' a
addPath l (ScopedLens sl) = ScopedLens (l . sl)

type LensMap s = LMap SensorKey (ScopedLens s (Maybe MySensor))

class HasSensors s where

  lsmap :: s -> LensMap s

instance (HasSensors (k, v)) => HasSensors (LMap k v)  where

  {-lsmap :: LMap k v -> LMap SensorKey (ScopedLens (Lmap k v) (Maybe MySensor))-}
  lsmap m = LM.fromList $ LM.toList m <&> \(k,v) -> _undefined (k, lsmap (k,v))

  {-lsmap m = LM.fromList . mconcat $ LM.toList m <&> (\(k,v) -> lsmap (k,v) <&> addPath (at k) )-}
  {-lsmap m :: LM.toList m <&> \(k,v) -> -}

instance HasSensors (Int, A)

instance HasSensors MyState where

  lsmap st = (mTruc <&> addPath (field @"truc")) <> (mChose <&> addPath (field @"chose"))
    where
      mTruc :: LensMap (LMap Int A)
      mTruc = lsmap (truc st)
      mChose :: LensMap (LMap Int B)
      mChose = undefined
      mMachin :: LensMap (LMap Int C)
      mMachin = undefined
