{-|

Module      : Nrm.Types.Sensor
Copyright   : (c) UChicago Argonne, 2019
License     : BSD3
Maintainer  : fre@freux.fr
-}
module Nrm.Types.Sensor
  ( Source (..)
  , Sensor (..)
  , SensorTag (..)
  )
where

import Nrm.Types.Metadata
import Nrm.Types.Process as P
import Nrm.Types.Topology as T
import Data.MessagePack
import Protolude

data SensorTag = SensorTag Text
  deriving (Show, Generic)

deriving instance MessagePack Sensor
deriving instance MessagePack SensorTag
deriving instance MessagePack Source

data Source = Process P.ProcessID | PU T.PUID
  deriving (Show, Generic)

data Sensor = Sensor SensorTag Source Range
  deriving (Show, Generic)
