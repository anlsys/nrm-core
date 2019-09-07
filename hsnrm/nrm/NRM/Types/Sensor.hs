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
  , HasSensors (..)
  , IsSensor (..)
  , PackedSensor
  , packSensor
  , toCPDPackedSensor
  , -- * Re-exports
    CPD.Source (..)
  , CPD.SensorID (..)
  )
where

import qualified CPD.Core as CPD
import qualified NRM.Types.Units as U
import Protolude
import NRM.Classes.Sensors

newtype Tag = Tag Text

data Sensor
  = PassiveSensor
      { perform :: IO (Maybe Double)
      , sensorTags :: [Tag]
      , frequency :: U.Frequency
      , source :: CPD.Source
      , range :: (Double, Double)
      , sensorDesc :: Maybe Text
      }
  | ActiveSensor
      { maxFrequency :: U.Frequency
      , sensorTags :: [Tag]
      , process :: Double -> Double
      , source :: CPD.Source
      , range :: (Double, Double)
      , sensorDesc :: Maybe Text
      }

instance IsSensor Sensor where

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
