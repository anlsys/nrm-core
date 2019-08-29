{-# LANGUAGE DerivingVia #-}

{-|

Module      : NRM.Types.Sensor
Copyright   : (c) UChicago Argonne, 2019
License     : BSD3
Maintainer  : fre@freux.fr
-}
module NRM.Types.Sensor
  ( -- * NRM internal view
    PackageSensor (..)
  , RaplSensor (..)
  , -- * Controller view
    Source (..)
  , Sensor (..)
  , SensorTag (..)
  , -- * Rendering
    showSensorList
  )
where

import Data.Aeson
import Data.JSON.Schema
import Data.MessagePack
import NRM.Classes.Messaging
import NRM.Types.Metadata
import NRM.Types.Process as P
import NRM.Types.Topology
import NRM.Types.Topology.Package
import Protolude

-- Internal view
data PackageSensor = TagRaplSensor RaplSensor
  deriving (Show, Generic, MessagePack, FromJSON, ToJSON)

-- Controller
data SensorTag = SensorTag Text
  deriving (Show, Generic, MessagePack)
  deriving (JSONSchema, ToJSON, FromJSON) via GenericJSON SensorTag

data Source = Process P.ProcessID | PU PUID
  deriving (Show, Generic, MessagePack)
  deriving (JSONSchema, ToJSON, FromJSON) via GenericJSON Source

data Sensor = Sensor SensorTag Source Range
  deriving (Show, Generic, MessagePack)
  deriving (JSONSchema, ToJSON, FromJSON) via GenericJSON Sensor

showSensorList :: [Sensor] -> Text
showSensorList = undefined
