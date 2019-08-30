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
  , Discrete (..)
  , Value (..)
  , Frequency (..)
  , -- * Sensors
    -- ** Classes
    CPDLSensor (..)
  , HasSensors (..)
  , -- ** Definitions
    SensorID (..)
  , Sensor (..)
  , Source (..)
  , Tag (..)
  , -- * Actuators
    -- ** Classes
    ActuatorID (..)
  , CPDLActuator (..)
  , -- ** Definitions
    Actuator (..)
  )
where

import qualified Data.Aeson as A
import Data.JSON.Schema
import Data.MessagePack
import qualified Data.UUID as U
import NRM.Classes.Messaging
import NRM.Orphans.UUID ()
import Protolude

-- Metadata
newtype Discrete = D Text
  deriving (Eq, Ord, Show, Read, Generic, MessagePack)
  deriving (JSONSchema, A.ToJSON, A.FromJSON) via GenericJSON Discrete

data Frequency = MaxFrequency Double | FixedFrequency Double
  deriving (Eq, Ord, Show, Read, Generic, MessagePack)
  deriving (JSONSchema, A.ToJSON, A.FromJSON) via GenericJSON Frequency

data Metadata = Metadata Range Frequency
  deriving (Eq, Ord, Show, Read, Generic, MessagePack)
  deriving (JSONSchema, A.ToJSON, A.FromJSON) via GenericJSON Metadata

data Range
  = Set [Discrete]
  | Interval Double Double
  deriving (Eq, Ord, Show, Read, Generic, MessagePack)
  deriving (JSONSchema, A.ToJSON, A.FromJSON) via GenericJSON Range

data Value
  = DiscreteValue Discrete
  | ContinuousValue Double
  deriving (Eq, Ord, Show, Read, Generic, MessagePack)
  deriving (JSONSchema, A.ToJSON, A.FromJSON) via GenericJSON Value

-------- SENSORS
class HasSensors a ownerContext | a -> ownerContext where

  toSensors :: ownerContext -> a -> Map SensorID Sensor

class CPDLSensor a sensorContext | a -> sensorContext where

  toSensor :: sensorContext -> a -> (SensorID, Sensor)

{-data EncapsulatedSensor = forall a b. CPDLSensor a b  => MkEncapsulatedSensor a-}

{-packSensor :: CPDLSensor a => a -> EncapsulatedSensor-}
{-packSensor = MkEncapsulatedSensor-}
newtype SensorID = SensorID U.UUID
  deriving (Show, Eq, Ord, Generic, MessagePack, A.ToJSONKey, A.FromJSONKey)
  deriving (JSONSchema, A.ToJSON, A.FromJSON) via GenericJSON SensorID

newtype Tag = Tag Text
  deriving (Show, Generic, MessagePack)
  deriving (JSONSchema, A.ToJSON, A.FromJSON) via GenericJSON Tag

data Source = Source Text
  deriving (Show, Generic, MessagePack)
  deriving (JSONSchema, A.ToJSON, A.FromJSON) via GenericJSON Source

data Sensor
  = Sensor
      { tags :: [Tag]
      , source :: Source
      , meta :: Metadata
      , desc :: Maybe Text
      }
  deriving (Show, Generic, MessagePack)
  deriving (JSONSchema, A.ToJSON, A.FromJSON) via GenericJSON Sensor

------- ACTUATORS
class CPDLActuator a where

  toActuator :: a -> Actuator

newtype ActuatorID = ActuatorID U.UUID
  deriving (Show, Eq, Ord, Generic, MessagePack, A.ToJSONKey, A.FromJSONKey)
  deriving (JSONSchema, A.ToJSON, A.FromJSON) via GenericJSON ActuatorID

data Actuator = Actuator
  deriving (Show, Generic, MessagePack)
  deriving (JSONSchema, A.ToJSON, A.FromJSON) via GenericJSON Actuator
