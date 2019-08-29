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
  , showSensor
  )
where

import Data.Aeson
import Data.JSON.Schema
import Data.MessagePack
import qualified Data.UUID as U
import NRM.Classes.Messaging
import NRM.Types.Metadata
import NRM.Types.Process
import NRM.Types.Topology
import NRM.Types.Topology.Package
import Protolude

newtype SensorID = SensorID U.UUID
  deriving (Show, Eq, Ord, Generic, MessagePack, ToJSONKey, FromJSONKey)
  deriving (JSONSchema, ToJSON, FromJSON) via GenericJSON SensorID

-- Internal view
newtype PackageSensor = TagRaplSensor RaplSensor
  deriving (Show, Generic, MessagePack, FromJSON, ToJSON)

-- Controller
newtype SensorTag = SensorTag Text
  deriving (Show, Generic, MessagePack)
  deriving (JSONSchema, ToJSON, FromJSON) via GenericJSON SensorTag

data Source = Process ProcessID | PU PUID
  deriving (Show, Generic, MessagePack)
  deriving (JSONSchema, ToJSON, FromJSON) via GenericJSON Source

data Sensor
  = Sensor
      { id :: SensorID
      , tags :: [SensorTag]
      , source :: Source
      , meta :: Range
      }
  deriving (Show, Generic, MessagePack)
  deriving (JSONSchema, ToJSON, FromJSON) via GenericJSON Sensor

showSensorList :: [Sensor] -> Text
showSensorList sl = mconcat $ (\x -> showSensor x <> " ") <$> sl

showSensor :: Sensor -> Text
showSensor (Sensor id tags source range) =
  "ID  " <> show id <>
    " tags:" <>
    (mconcat . intersperse " " $ show <$> tags) <>
    " " <>
    show source <>
    " " <>
    show range <>
    " \n"
