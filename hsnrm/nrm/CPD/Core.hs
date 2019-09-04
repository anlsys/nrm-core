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
  deriving (JSONSchema, A.ToJSON, A.FromJSON) via GenericJSON Value
  deriving (Show, Generic, MessagePack, Interpret, Inject)

-------- SENSORS
newtype SensorID = SensorID {sensorID :: U.UUID}
  deriving (JSONSchema, A.ToJSON, A.FromJSON) via GenericJSON SensorID
  deriving
    ( Ord
    , Eq
    , Show
    , Generic
    , MessagePack
    , A.ToJSONKey
    , A.FromJSONKey
    , Interpret
    , Inject
    )

nextSensorID :: IO SensorID
nextSensorID = UV4.nextRandom <&> SensorID

newtype Source = Source {sourceTag :: Text}
  deriving (Show, Generic, MessagePack)
  deriving (JSONSchema, A.ToJSON, A.FromJSON) via GenericJSON Source
  deriving (Interpret, Inject) via Text

data Sensor
  = Sensor
      { source :: Source
      , sensorMeta :: Metadata
      }
  deriving (Show, Generic, MessagePack, Interpret, Inject)
  deriving (JSONSchema, A.ToJSON, A.FromJSON) via GenericJSON Sensor

------- ACTUATORS
class CPDLActuator a where

  toActuator :: a -> Actuator

newtype ActuatorID = ActuatorID {actuatorID :: U.UUID}
  deriving
    ( Ord
    , Eq
    , Show
    , Generic
    , MessagePack
    , A.ToJSONKey
    , A.FromJSONKey
    , Interpret
    , Inject
    )
  deriving (JSONSchema, A.ToJSON, A.FromJSON) via GenericJSON ActuatorID

nextActuatorID :: IO ActuatorID
nextActuatorID = UV4.nextRandom <&> ActuatorID

newtype Target = Target {targetTag :: Text}
  deriving (JSONSchema, A.ToJSON, A.FromJSON) via GenericJSON Source
  deriving (Show, Generic, MessagePack)
  deriving (Interpret, Inject) via Text

newtype ActuatorMetadata = ActuatorMetadata {actuatorRange :: Range}
  deriving (JSONSchema, A.ToJSON, A.FromJSON) via GenericJSON ActuatorMetadata
  deriving (Show, Generic, MessagePack)
  deriving (Interpret, Inject) via Range

data Actuator
  = Actuator
      { target :: Target
      , actuatorMeta :: ActuatorMetadata
      }
  deriving (JSONSchema, A.ToJSON, A.FromJSON) via GenericJSON Actuator
  deriving (Show, Generic, MessagePack, Interpret, Inject)

------- OBJECTIVE
data Direction = Minimize | Maximize
  deriving (JSONSchema, A.ToJSON, A.FromJSON) via GenericJSON Direction
  deriving (Show, Generic, MessagePack, Interpret, Inject)

data X = X {w :: Double, x :: SensorID}
  deriving (JSONSchema, A.ToJSON, A.FromJSON) via GenericJSON X
  deriving (Show, Generic, MessagePack, Interpret, Inject)

data Objective
  = Objective
      { linearCombination :: [X]
      , direction :: Direction
      }
  deriving (JSONSchema, A.ToJSON, A.FromJSON) via GenericJSON Objective
  deriving (Show, Generic, MessagePack, Interpret, Inject)