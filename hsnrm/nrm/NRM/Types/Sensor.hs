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
  , -- * Re-exports
    CPD.Source (..)
  , CPD.SensorID (..)
  )
where

import qualified CPD.Core as CPD
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

data ActiveSensor
  = ActiveSensor
      { maxFrequency :: U.Frequency
      , activeTags :: [Tag]
      , process :: Double -> Double
      , activeSource :: CPD.Source
      , activeRange :: (Double, Double)
      }
