{-# LANGUAGE RankNTypes #-}

{-|
Module      : NRM.Classes.Sensors
Copyright   : (c) UChicago Argonne, 2019
License     : BSD3
Maintainer  : fre@freux.fr
-}
module NRM.Classes.Sensors
  ( ToCPDSensor (..)
  , ToCPDKey (..)
  )
where

import qualified CPD.Core as CPD
import Control.Lens
import Data.Generics.Product
import LensMap.Core
import NRM.Types.LMap as LM
import Protolude

class ToCPDSensor k a where

  toCPDSensor :: (k, a) -> (CPD.SensorID, CPD.Sensor)

class ToCPDKey k where

  toKey :: k -> CPD.SensorID
