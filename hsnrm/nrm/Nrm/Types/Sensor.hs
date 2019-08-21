{-|

Module      : Nrm.Types.Sensor
Copyright   : (c) UChicago Argonne, 2019
License     : BSD3
Maintainer  : fre@freux.fr
-}
module Nrm.Types.Sensor
  ( -- * Nrm internal view
    PackageSensor (..)
  , RaplSensor (..)
  , -- * Controller view
    Source (..)
  , Sensor (..)
  , SensorTag (..)
  )
where

import Data.MessagePack
import Nrm.Node.Sysfs.Internal
import Nrm.Types.Metadata
import Nrm.Types.Process as P
import Nrm.Types.Topology
import Nrm.Types.Topology.Package
import Protolude

-- Internal view

data PackageSensor = TagRaplSensor RaplSensor

-- Controller
data SensorTag = SensorTag Text
  deriving (Show, Generic, MessagePack)

data Source = Process P.ProcessID | PU PUID
  deriving (Show, Generic, MessagePack)

data Sensor = Sensor SensorTag Source Range
  deriving (Show, Generic, MessagePack)
