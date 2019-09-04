{-# LANGUAGE DerivingVia #-}

{-|
Module      : CPD.Core
Copyright   : (c) UChicago Argonne, 2019
License     : BSD3
Maintainer  : fre@freux.fr
-}
module CPD.Core
  ( -- * Metadata
    Problem (..)
  , Metadata (..)
  , Range (..)
  , Value (..)
  , Frequency (..)
  , -- * Sensors
    -- ** Definitions
    SensorKV (..)
  , SensorID (..)
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
    ActuatorKV (..)
  , Actuator (..)
  , -- * Objective
    Objective (..)
  , X (..)
  , Direction (..)
  )
where

import qualified Data.Aeson as A
import Data.JSON.Schema
{-import Data.Maybe (fromJust)-}
import Data.MessagePack
import qualified Data.UUID as U
import qualified Data.UUID.V4 as UV4
import Dhall
import NRM.Classes.Messaging
import NRM.Orphans.UUID ()
import qualified NRM.Types.Units as Units
import Protolude

-- Metadata
type Discrete = Text

data Problem
  = Problem
      { sensors :: [SensorKV]
      , actuators :: [ActuatorKV]
      , objective :: Objective
      }
  deriving (Show, Generic, MessagePack, Interpret, Inject)
  deriving (JSONSchema, A.ToJSON, A.FromJSON) via GenericJSON Problem

data ActuatorKV
  = ActuatorKV
      { actuatorKey :: ActuatorID
      , actuatorValue :: Actuator
      }
  deriving (Show, Generic, MessagePack, Interpret, Inject)
  deriving (JSONSchema, A.ToJSON, A.FromJSON) via GenericJSON ActuatorKV

data SensorKV
  = SensorKV
      { sensorKey :: SensorID
      , sensorValue :: Sensor
      }
  deriving (Show, Generic, MessagePack, Interpret, Inject)
  deriving (JSONSchema, A.ToJSON, A.FromJSON) via GenericJSON SensorKV

data Frequency
  = MaxFrequency {maxFrequency :: Units.Frequency}
  | FixedFrequency {fixedFrequency :: Units.Frequency}
  deriving (Show, Generic, MessagePack, Interpret, Inject)
  deriving (JSONSchema, A.ToJSON, A.FromJSON) via GenericJSON Frequency

data Metadata = Metadata {range :: Range, frequency :: Frequency}
  deriving (Show, Generic, MessagePack, Interpret, Inject)
  deriving (JSONSchema, A.ToJSON, A.FromJSON) via GenericJSON Metadata

data Range
  = Set {admissibleValues :: [Discrete]}
  | Interval {mix :: Double, max :: Double}
  deriving (Show, Generic, MessagePack, Interpret, Inject)
  deriving (JSONSchema, A.ToJSON, A.FromJSON) via GenericJSON Range

data Value
  = DiscreteValue {discreteValue :: Discrete}
  | ContinuousValue {continuousValue :: Double}
  deriving (Show, Generic, MessagePack, Interpret, Inject)
  deriving (JSONSchema, A.ToJSON, A.FromJSON) via GenericJSON Value

-------- SENSORS
newtype SensorID = SensorID {sensorID :: U.UUID}
  deriving (Ord, Eq, Show, Generic, MessagePack, A.ToJSONKey, A.FromJSONKey)
  deriving (JSONSchema, A.ToJSON, A.FromJSON) via GenericJSON SensorID
  deriving (Interpret, Inject) via U.UUID

nextSensorID :: IO SensorID
nextSensorID = UV4.nextRandom <&> SensorID

newtype Tag = Tag {tag :: Text}
  deriving (Show, Generic, MessagePack)
  deriving (JSONSchema, A.ToJSON, A.FromJSON) via GenericJSON Tag
  deriving (Interpret, Inject) via Text

newtype Source = Source {sourceTag :: Text}
  deriving (Show, Generic, MessagePack)
  deriving (JSONSchema, A.ToJSON, A.FromJSON) via GenericJSON Source
  deriving (Interpret, Inject) via Text

data Sensor
  = Sensor
      { sensorTags :: [Tag]
      , source :: Source
      , sensorMeta :: Metadata
      , sensorDesc :: Maybe Text
      }
  deriving (Show, Generic, MessagePack, Interpret, Inject)
  deriving (JSONSchema, A.ToJSON, A.FromJSON) via GenericJSON Sensor

------- ACTUATORS
class CPDLActuator a where

  toActuator :: a -> Actuator

newtype ActuatorID = ActuatorID {actuatorID :: U.UUID}
  deriving (Ord, Eq, Show, Generic, MessagePack, A.ToJSONKey, A.FromJSONKey)
  deriving (JSONSchema, A.ToJSON, A.FromJSON) via GenericJSON ActuatorID
  deriving (Interpret, Inject) via U.UUID

nextActuatorID :: IO ActuatorID
nextActuatorID = UV4.nextRandom <&> ActuatorID

newtype Target = Target {targetTag :: Text}
  deriving (Show, Generic, MessagePack)
  deriving (JSONSchema, A.ToJSON, A.FromJSON) via GenericJSON Source
  deriving (Interpret, Inject) via Text

newtype ActuatorMetadata = ActuatorMetadata {actuatorRange :: Range}
  deriving (Show, Generic, MessagePack)
  deriving (JSONSchema, A.ToJSON, A.FromJSON) via GenericJSON ActuatorMetadata
  deriving (Interpret, Inject) via Range

data Actuator
  = Actuator
      { actuatorTags :: [Tag]
      , target :: Target
      , actuatorMeta :: ActuatorMetadata
      , actuatorDesc :: Maybe Text
      }
  deriving (Show, Generic, MessagePack, Interpret, Inject)
  deriving (JSONSchema, A.ToJSON, A.FromJSON) via GenericJSON Actuator

------- OBJECTIVE
data Direction = Minimize | Maximize
  deriving (Show, Generic, MessagePack, Interpret, Inject)
  deriving (JSONSchema, A.ToJSON, A.FromJSON) via GenericJSON Direction

data X = X {w :: Double, x :: Source}
  deriving (Show, Generic, MessagePack, Interpret, Inject)
  deriving (JSONSchema, A.ToJSON, A.FromJSON) via GenericJSON X

data Objective
  = Objective
      { linearCombination :: [X]
      , direction :: Direction
      }
  deriving (Show, Generic, MessagePack, Interpret, Inject)
  deriving (JSONSchema, A.ToJSON, A.FromJSON) via GenericJSON Objective
