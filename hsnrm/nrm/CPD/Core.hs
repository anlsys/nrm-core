{-# LANGUAGE DerivingVia #-}

{-|
Module      : CPD.Core
Copyright   : (c) UChicago Argonne, 2019
License     : BSD3
Maintainer  : fre@freux.fr
-}
module CPD.Core
  ( -- * Metadata
    Metadata (..)
  , Range (..)
  , Value (..)
  , Frequency (..)
  , -- * Sensors
    -- ** Definitions
    SensorID (..)
  , nextSensorID
  , Sensor (..)
  , Source (..)
  , Tag (..)
  , -- * Actuators
    -- ** Classes
    ActuatorID (..)
  , nextActuatorID
  , CPDLActuator (..)
  , -- ** Definitions
    Actuator (..)
  )
where

import qualified Data.Aeson as A
import Data.JSON.Schema
import Data.Maybe (fromJust)
import Data.MessagePack
import qualified Data.UUID as U
import qualified Data.UUID.V4 as UV4
import Dhall
import NRM.Classes.Messaging
import NRM.Orphans.UUID ()
import Protolude

-- Metadata
type Discrete = Text

data Frequency = MaxFrequency Double | FixedFrequency Double
  deriving (Show, Read, Generic, MessagePack, Interpret)
  deriving (JSONSchema, A.ToJSON, A.FromJSON) via GenericJSON Frequency

data Metadata = Metadata Range Frequency
  deriving (Show, Read, Generic, MessagePack, Interpret)
  deriving (JSONSchema, A.ToJSON, A.FromJSON) via GenericJSON Metadata

data Range
  = Set {discreteRange :: [Discrete]}
  | Interval {mix :: Double, max :: Double}
  deriving (Show, Read, Generic, MessagePack, Interpret)
  deriving (JSONSchema, A.ToJSON, A.FromJSON) via GenericJSON Range

data Value
  = DiscreteValue {discreteValue :: Discrete}
  | ContinuousValue {continuousValue :: Double}
  deriving (Show, Read, Generic, MessagePack, Interpret)
  deriving (JSONSchema, A.ToJSON, A.FromJSON) via GenericJSON Value

-------- SENSORS
newtype SensorID = SensorID {sensorID :: U.UUID}
  deriving (Ord, Eq, Show, Generic, MessagePack, A.ToJSONKey, A.FromJSONKey)
  deriving (JSONSchema, A.ToJSON, A.FromJSON) via GenericJSON SensorID

instance Interpret SensorID where

  autoWith = fmap (SensorID . fromJust . U.fromText) . autoWith

nextSensorID :: IO SensorID
nextSensorID = UV4.nextRandom <&> SensorID

newtype Tag = Tag {tag :: Text}
  deriving (Show, Generic, MessagePack, Interpret)
  deriving (JSONSchema, A.ToJSON, A.FromJSON) via GenericJSON Tag

newtype Source = Source {sourceTag :: Text}
  deriving (Show, Generic, MessagePack, Interpret)
  deriving (JSONSchema, A.ToJSON, A.FromJSON) via GenericJSON Source

data Sensor
  = Sensor
      { tags :: [Tag]
      , source :: Source
      , meta :: Metadata
      , desc :: Maybe Text
      }
  deriving (Show, Generic, MessagePack, Interpret)
  deriving (JSONSchema, A.ToJSON, A.FromJSON) via GenericJSON Sensor

------- ACTUATORS
class CPDLActuator a where

  toActuator :: a -> Actuator

newtype ActuatorID = ActuatorID U.UUID
  deriving (Ord, Eq, Show, Generic, MessagePack, A.ToJSONKey, A.FromJSONKey)
  deriving (JSONSchema, A.ToJSON, A.FromJSON) via GenericJSON ActuatorID

nextActuatorID :: IO ActuatorID
nextActuatorID = UV4.nextRandom <&> ActuatorID

data Actuator = Actuator
  deriving (Show, Generic, MessagePack, Interpret)
  deriving (JSONSchema, A.ToJSON, A.FromJSON) via GenericJSON Actuator
