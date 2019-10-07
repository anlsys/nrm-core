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
  , emptyProblem
  , Metadata (..)
  , Interval (..)
  , Admissible (..)
  , Discrete (..)
  , Frequency (..)
  , -- * Sensors
    -- ** Definitions
    SensorID (..)
  , Sensor (..)
  , Source (..)
  , -- * Actuators
    -- ** Classes
    ActuatorID (..)
  , CPDLActuator (..)
  , -- ** Definitions
    Actuator (..)
  , -- * Objective
    Objective (..)
  , X (..)
  , Direction (..)
  )
where

import qualified Data.Aeson as A
import Data.Data
import Data.JSON.Schema
import qualified Data.Map as DM
import Data.MessagePack
import Data.String (IsString (..))
import Dhall
import NRM.Classes.Messaging
import NRM.Orphans.UUID ()
import qualified NRM.Types.Units as Units
import Protolude

-- METADATA
data Discrete = DiscreteText Text | DiscreteDouble Double
  deriving
    ( Show
    , Eq
    , Generic
    , Data
    , MessagePack
    , Interpret
    , Inject
    )
  deriving (JSONSchema, A.ToJSON, A.FromJSON) via GenericJSON Discrete

data Problem
  = Problem
      { sensors :: Map SensorID Sensor
      , actuators :: Map ActuatorID Actuator
      , objective :: Objective
      }
  deriving (Show, Generic, Data, MessagePack, Interpret, Inject)
  deriving (JSONSchema, A.ToJSON, A.FromJSON) via GenericJSON Problem

emptyProblem :: Problem
emptyProblem = Problem DM.empty DM.empty emptyObjective

newtype Frequency
  = MaxFrequency {maxFrequency :: Units.Frequency}
  deriving (Show, Generic, Data, MessagePack, Interpret, Inject)
  deriving (JSONSchema, A.ToJSON, A.FromJSON) via GenericJSON Frequency

data Metadata = Metadata {range :: Interval, frequency :: Frequency}
  deriving (Show, Generic, Data, MessagePack, Interpret, Inject)
  deriving (JSONSchema, A.ToJSON, A.FromJSON) via GenericJSON Metadata

data Interval
  = Interval {min :: Double, max :: Double}
  deriving (Show, Generic, Data, MessagePack, Interpret, Inject)
  deriving (JSONSchema, A.ToJSON, A.FromJSON) via GenericJSON Interval

newtype Admissible = Admissible {admissibleValues :: [Discrete]}
  deriving (Show, Generic, Data, MessagePack, Interpret, Inject)
  deriving (JSONSchema, A.ToJSON, A.FromJSON) via GenericJSON Admissible

-------- SENSORS
newtype SensorID = SensorID {sensorID :: Text}
  deriving (JSONSchema, A.ToJSON, A.FromJSON) via GenericJSON SensorID
  deriving
    ( Ord
    , Eq
    , Show
    , Generic
    , Data
    , MessagePack
    , A.ToJSONKey
    , A.FromJSONKey
    , Interpret
    , Inject
    )
  deriving (IsString) via Text

newtype Source = Source {sourceTag :: Text}
  deriving (Show, Generic, Data, MessagePack)
  deriving (JSONSchema, A.ToJSON, A.FromJSON) via GenericJSON Source
  deriving (Interpret, Inject) via Text

data Sensor
  = Sensor
      { source :: Source
      , sensorMeta :: Metadata
      }
  deriving (Show, Generic, Data, MessagePack, Interpret, Inject)
  deriving (JSONSchema, A.ToJSON, A.FromJSON) via GenericJSON Sensor

------- ACTUATORS
class CPDLActuator a where

  toActuator :: a -> Actuator

newtype ActuatorID = ActuatorID {actuatorID :: Text}
  deriving
    ( Ord
    , Eq
    , Show
    , Generic
    , Data
    , MessagePack
    , A.ToJSONKey
    , A.FromJSONKey
    , Interpret
    , Inject
    )
  deriving (JSONSchema, A.ToJSON, A.FromJSON) via GenericJSON ActuatorID

instance IsString ActuatorID where

  fromString x = fromMaybe (panic "couldn't decode ActuatorID in FromString instance") (A.decode $ toS x)

newtype Actuator = Actuator {actuatorRange :: [Discrete]}
  deriving (JSONSchema, A.ToJSON, A.FromJSON) via GenericJSON Actuator
  deriving (Show, Generic, Data, MessagePack)
  deriving (Interpret, Inject) via [Discrete]

------- OBJECTIVE
data Direction = Minimize | Maximize
  deriving (JSONSchema, A.ToJSON, A.FromJSON) via GenericJSON Direction
  deriving (Show, Eq, Generic, Data, MessagePack, Interpret, Inject)

data X = X {w :: Double, x :: SensorID}
  deriving (JSONSchema, A.ToJSON, A.FromJSON) via GenericJSON X
  deriving (Show, Eq, Generic, Data, MessagePack, Interpret, Inject)

data Objective
  = Objective
      { linearCombination :: [X]
      , direction :: Direction
      }
  deriving (JSONSchema, A.ToJSON, A.FromJSON) via GenericJSON Objective
  deriving (Show, Eq, Generic, Data, MessagePack, Interpret, Inject)

emptyObjective :: Objective
emptyObjective = Objective [] Minimize
