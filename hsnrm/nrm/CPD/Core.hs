{-# LANGUAGE DerivingVia #-}
{-# OPTIONS_GHC -fno-warn-orphans #-}

-- |
-- Module      : CPD.Core
-- Copyright   : (c) UChicago Argonne, 2019
-- License     : BSD3
-- Maintainer  : fre@freux.fr
module CPD.Core
  ( -- * Metadata
    Problem (..),
    emptyProblem,
    Metadata (..),
    Interval,
    Admissible (..),
    Discrete (..),
    Frequency (..),

    -- * Sensors

    -- ** Definitions
    SensorID (..),
    Sensor (..),
    Source (..),

    -- * Actuators

    -- ** Classes
    ActuatorID (..),
    CPDLActuator (..),

    -- ** Definitions
    Actuator (..),

    -- * Objective
    Objective,
    OExpr (..),
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
import Numeric.Interval as I hiding (elem)
import Protolude

-- METADATA
data Discrete = DiscreteText Text | DiscreteDouble Double
  deriving
    ( Show,
      Eq,
      Generic,
      Data,
      MessagePack,
      Interpret,
      Inject
    )
  deriving (JSONSchema, A.ToJSON, A.FromJSON) via GenericJSON Discrete

data Problem
  = Problem
      { sensors :: Map SensorID Sensor,
        actuators :: Map ActuatorID Actuator,
        objective :: Objective
      }
  deriving (Show, Generic, Data, MessagePack, Interpret, Inject)
  deriving (JSONSchema, A.ToJSON, A.FromJSON) via GenericJSON Problem

emptyProblem :: Problem
emptyProblem = Problem DM.empty DM.empty emptyObjective

newtype Frequency
  = MaxFrequency {maxFrequency :: Units.Frequency}
  deriving (Show, Generic, Data, MessagePack, Interpret, Inject)
  deriving (JSONSchema, A.ToJSON, A.FromJSON) via GenericJSON Frequency

data Metadata = Metadata {range :: Interval Double, frequency :: Frequency}
  deriving (Show, Generic, Data, MessagePack, Interpret, Inject)
  deriving (JSONSchema, A.ToJSON, A.FromJSON) via GenericJSON Metadata

deriving instance MessagePack (Interval Double)

deriving instance Interpret (Interval Double)

deriving instance Inject (Interval Double)

deriving via GenericJSON (Interval Double) instance JSONSchema (Interval Double)

deriving via GenericJSON (Interval Double) instance A.ToJSON (Interval Double)

deriving via GenericJSON (Interval Double) instance A.FromJSON (Interval Double)

newtype Admissible = Admissible {admissibleValues :: [Discrete]}
  deriving (Show, Generic, Data, MessagePack, Interpret, Inject)
  deriving (JSONSchema, A.ToJSON, A.FromJSON) via GenericJSON Admissible

-------- SENSORS
newtype SensorID = SensorID {sensorID :: Text}
  deriving (JSONSchema, A.ToJSON, A.FromJSON) via GenericJSON SensorID
  deriving
    ( Ord,
      Eq,
      Show,
      Generic,
      Data,
      MessagePack,
      A.ToJSONKey,
      A.FromJSONKey,
      Interpret,
      Inject
    )
  deriving (IsString) via Text

newtype Source = Source {sourceTag :: Text}
  deriving (Show, Generic, Data, MessagePack)
  deriving (JSONSchema, A.ToJSON, A.FromJSON) via GenericJSON Source
  deriving (Interpret, Inject) via Text

data Sensor
  = Sensor
      { source :: Source,
        sensorMeta :: Metadata
      }
  deriving (Show, Generic, Data, MessagePack, Interpret, Inject)
  deriving (JSONSchema, A.ToJSON, A.FromJSON) via GenericJSON Sensor

------- ACTUATORS
class CPDLActuator a where
  toActuator :: a -> Actuator

newtype ActuatorID = ActuatorID {actuatorID :: Text}
  deriving
    ( Ord,
      Eq,
      Show,
      Generic,
      Data,
      MessagePack,
      A.ToJSONKey,
      A.FromJSONKey,
      Interpret,
      Inject
    )
  deriving (JSONSchema, A.ToJSON, A.FromJSON) via GenericJSON ActuatorID

instance IsString ActuatorID where
  fromString x = fromMaybe (panic "couldn't decode ActuatorID in FromString instance") (A.decode $ toS x)

newtype Actuator = Actuator {actions :: [Discrete]}
  deriving (JSONSchema, A.ToJSON, A.FromJSON) via GenericJSON Actuator
  deriving (Show, Generic, Data, MessagePack)
  deriving (Interpret, Inject) via [Discrete]

------- OBJECTIVE
type Objective = Maybe OExpr

--deriving (JSONSchema, A.ToJSON, A.FromJSON) via GenericJSON Objective
--deriving (Show, Eq, Generic, Data, MessagePack, Interpret, Inject)
emptyObjective :: Objective
emptyObjective = Nothing

data OExpr
  = OValue SensorID
  | OScalarMult Double OExpr
  | OAdd OExpr OExpr
  | OSub OExpr OExpr
  | OMul OExpr OExpr
  | ODiv OExpr OExpr
  deriving (JSONSchema, A.ToJSON, A.FromJSON) via GenericJSON OExpr
  deriving (Show, Eq, Generic, Data, MessagePack, Interpret, Inject)
