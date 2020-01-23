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
    Interval,
    Admissible (..),
    Discrete (..),

    -- * Sensors

    -- ** Definitions
    SensorID (..),
    Sensor (..),

    -- * Actuators

    -- ** Classes
    ActuatorID (..),
    CPDLActuator (..),

    -- ** Definitions
    Actuator (..),

    -- * Objective/Constraint Epxression language
    OExpr (..),

    -- * Objective contstructor helpers
    sID,
    scalar,
    (\+),
    (\-),
    (\/),
    (\*),

    -- * Pretty printers
    prettyCPD,
    prettyExpr,
  )
where

import qualified Data.Aeson as A
import Data.JSON.Schema
import Data.MessagePack
import Data.String (IsString (..))
import qualified Dhall as D
import LMap.Map as Map
import NRM.Classes.Messaging
import NRM.Orphans.UUID ()
import NRM.Orphans.ZeroOne ()
import qualified NRM.Types.Units as Units
import NeatInterpolation
import Numeric.Interval as I hiding (elem)
import Protolude hiding (Map)

-- METADATA
newtype Discrete = DiscreteDouble Double
  deriving
    ( Show,
      Eq,
      Generic,
      MessagePack,
      D.Interpret,
      D.Inject
    )
  deriving (JSONSchema, A.ToJSON, A.FromJSON) via GenericJSON Discrete

data Problem
  = Problem
      { sensors :: Map SensorID Sensor,
        actuators :: Map ActuatorID Actuator,
        objectives :: [(Double, OExpr)],
        constraints :: [(Interval Double, OExpr)]
      }
  deriving (Show, Generic, MessagePack, D.Interpret, D.Inject)
  deriving (JSONSchema, A.ToJSON, A.FromJSON) via GenericJSON Problem

emptyProblem :: Problem
emptyProblem = Problem Map.empty Map.empty [] []

data Sensor = Sensor {range :: Interval Double, maxFrequency :: Units.Frequency}
  deriving (Show, Generic, MessagePack, D.Interpret, D.Inject)
  deriving (JSONSchema, A.ToJSON, A.FromJSON) via GenericJSON Sensor

deriving instance MessagePack (Interval Double)

deriving instance D.Interpret (Interval Double)

deriving instance D.Inject (Interval Double)

deriving via GenericJSON (Interval Double) instance JSONSchema (Interval Double)

deriving via GenericJSON (Interval Double) instance A.ToJSON (Interval Double)

deriving via GenericJSON (Interval Double) instance A.FromJSON (Interval Double)

newtype Admissible = Admissible {admissibleValues :: [Discrete]}
  deriving (Show, Generic, MessagePack, D.Interpret, D.Inject)
  deriving (JSONSchema, A.ToJSON, A.FromJSON) via GenericJSON Admissible

-------- SENSORS
newtype SensorID = SensorID {sensorID :: Text}
  deriving
    ( Ord,
      Eq,
      Show,
      Generic,
      MessagePack,
      A.ToJSONKey,
      A.FromJSONKey,
      D.Interpret,
      D.Inject
    )
  deriving (IsString, JSONSchema, A.ToJSON, A.FromJSON) via Text

------- ACTUATORS
class CPDLActuator a where
  toActuator :: a -> Actuator

newtype ActuatorID = ActuatorID {actuatorID :: Text}
  deriving
    ( Ord,
      Eq,
      Show,
      Read,
      Generic,
      MessagePack,
      A.ToJSONKey,
      A.FromJSONKey,
      D.Interpret,
      D.Inject
    )
  deriving (JSONSchema, A.ToJSON, A.FromJSON) via GenericJSON ActuatorID

instance IsString ActuatorID where
  fromString x =
    fromMaybe
      ( panic
          "couldn't decode ActuatorID in FromString instance"
      )
      (A.decode $ toS x)

newtype Actuator = Actuator {actions :: [Discrete]}
  deriving (JSONSchema, A.ToJSON, A.FromJSON) via GenericJSON Actuator
  deriving (Show, Generic, MessagePack)
  deriving (D.Interpret, D.Inject) via [Discrete]

------- OBJECTIVE
sID :: SensorID -> OExpr
sID = OValue

scalar :: Double -> OExpr
scalar = OScalar

(\+) :: OExpr -> OExpr -> OExpr
(\+) = OAdd

(\-) :: OExpr -> OExpr -> OExpr
(\-) = OSub

(\/) :: OExpr -> OExpr -> OExpr
(\/) = ODiv

(\*) :: OExpr -> OExpr -> OExpr
(\*) = OMul

data OExpr
  = OValue SensorID
  | OScalar Double
  | OAdd OExpr OExpr
  | OSub OExpr OExpr
  | OMul OExpr OExpr
  | ODiv OExpr OExpr
  deriving (A.ToJSON, A.FromJSON) via GenericJSON OExpr
  deriving (JSONSchema) via AnyJSON OExpr
  deriving (Show, Eq, Generic, MessagePack, D.Interpret, D.Inject)

prettyExpr :: OExpr -> Text
prettyExpr = \case
  OValue (SensorID id) -> "\"" <> id <> "\""
  OScalar d -> show d
  OAdd a b -> prettyExpr a <> "+" <> prettyExpr b
  OSub a b -> "(" <> prettyExpr a <> "-" <> prettyExpr b <> ")"
  OMul (OScalar d) b -> show d <> "*(" <> prettyExpr b <> ")"
  OMul a b -> "(" <> prettyExpr a <> ")*(" <> prettyExpr b <> ")"
  ODiv a b -> "(" <> prettyExpr a <> ")/(" <> prettyExpr b <> ")"

prettyCPD :: Problem -> Text
prettyCPD (Problem sensors actuators objectives constraints) =
  [text|
  Sensors : $s
  Actuators : $a
  Objectives : $objs
  Constraints : $csts
  |]
  where
    s = show sensors
    a = show actuators
    objs = mconcat . intersperse " " $ objectives <&> \(w, o) -> show w <> prettyExpr o
    csts = mconcat . intersperse " " $ constraints <&> \(w, c) -> show w <> prettyExpr c
