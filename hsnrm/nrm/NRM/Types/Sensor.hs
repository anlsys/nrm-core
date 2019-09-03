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
  , HasSensors (..)
  , IsSensor (..)
  , packSensor
  , toCPDPackedSensor
  , -- * Re-exports
    CPD.Tag (..)
  , CPD.Source (..)
  , CPD.SensorID (..)
  )
where

import qualified CPD.Core as CPD
import qualified NRM.Types.Units as U
import Protolude

data Sensor a
  = PassiveSensor
      { perform :: IO (Maybe Double)
      , frequency :: U.Frequency
      , sensorTags :: [CPD.Tag]
      , source :: CPD.Source
      , range :: (Double, Double)
      , sensorDesc :: Maybe Text
      }
  | ActiveSensor
      { maxFrequency :: U.Frequency
      , process :: Double -> Double
      , sensorTags :: [CPD.Tag]
      , source :: CPD.Source
      , range :: (Double, Double)
      , sensorDesc :: Maybe Text
      }

class IsSensor a where

  toCPDSensor :: CPD.SensorID -> a -> (CPD.SensorID, CPD.Sensor)

instance IsSensor (Sensor a) where

  toCPDSensor id PassiveSensor {..} =
    ( id
    , CPD.Sensor
      { sensorMeta = CPD.Metadata (uncurry CPD.Interval range)
          (CPD.FixedFrequency frequency)
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

-- Internal (NRM) classes
data PackedSensor = forall a. IsSensor a => MkPackedSensor a

packSensor :: IsSensor a => a -> PackedSensor
packSensor = MkPackedSensor

class HasSensors a aCtx | a -> aCtx where

  listSensors :: aCtx -> a -> Map CPD.SensorID PackedSensor

toCPDPackedSensor :: CPD.SensorID -> PackedSensor -> (CPD.SensorID, CPD.Sensor)
toCPDPackedSensor id (MkPackedSensor s) = toCPDSensor id s
