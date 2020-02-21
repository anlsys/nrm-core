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
    sRef,
    scalar,
    (\+),
    (\-),
    (\/),
    (\*),
    thresholded,

    -- * Pretty printers
    prettyCPD,
    prettyExpr,

    -- * Useful Newtypes
    OExprSum (..),
  )
where

import qualified Data.Aeson as A
import Data.Coerce
import Data.JSON.Schema
import Data.MessagePack
import qualified Dhall as D
import HBandit.Types
import LMap.Map as Map
import NRM.Classes.Messaging
import NRM.Orphans.UUID ()
import NRM.Orphans.ZeroOne ()
import qualified NRM.Types.Units as Units
import NeatInterpolation
import Numeric.Interval as I hiding (elem)
import Protolude hiding (Map)
import Refined

-- METADATA
newtype Discrete = DiscreteDouble Double
  deriving
    ( Show,
      Eq,
      Ord,
      Generic,
      MessagePack
    )
  deriving (JSONSchema, A.ToJSON, A.FromJSON, D.Interpret, D.Inject) via Double

data Problem
  = Problem
      { sensors :: Map SensorID Sensor,
        actuators :: Map ActuatorID Actuator,
        objectives :: [(ZeroOne Double, OExpr)],
        constraints :: [(Double, OExpr)]
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
      A.FromJSONKey
    )
  deriving
    ( JSONSchema,
      A.ToJSON,
      A.FromJSON,
      D.Interpret,
      D.Inject
    )
    via Text

newtype Actuator = Actuator {actions :: [Discrete]}
  deriving (JSONSchema, A.ToJSON, A.FromJSON) via GenericJSON Actuator
  deriving (Show, Generic, MessagePack)
  deriving (D.Interpret, D.Inject) via [Discrete]

------- OBJECTIVE
sID :: SensorID -> OExpr
sID = OValue

sRef :: SensorID -> OExpr
sRef = OReference

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

thresholded :: Double -> Double -> OExpr -> OExpr
thresholded low high e = OMax (scalar low) (OMin (scalar high) e)

data OExpr
  = OValue SensorID
  | OReference SensorID
  | OScalar Double
  | OAdd OExpr OExpr
  | OSub OExpr OExpr
  | OMul OExpr OExpr
  | ODiv OExpr OExpr
  | OMin OExpr OExpr
  | OMax OExpr OExpr
  deriving (A.ToJSON, A.FromJSON) via GenericJSON OExpr
  deriving (JSONSchema) via AnyJSON OExpr
  deriving (Show, Eq, Generic, MessagePack, D.Interpret, D.Inject)

prettyExpr :: OExpr -> Text
prettyExpr = \case
  OValue (SensorID id) -> "\"" <> id <> "\""
  OReference (SensorID id) -> "\"REF:" <> id <> "\""
  OScalar d -> show d
  OAdd a b -> prettyExpr a <> "+" <> prettyExpr b
  OSub a b -> "(" <> prettyExpr a <> "-" <> prettyExpr b <> ")"
  OMul (OScalar d) b -> show d <> "*(" <> prettyExpr b <> ")"
  OMul a b -> "(" <> prettyExpr a <> ")*(" <> prettyExpr b <> ")"
  ODiv a b -> "(" <> prettyExpr a <> ")/(" <> prettyExpr b <> ")"
  OMin a b -> "min(" <> prettyExpr a <> "," <> prettyExpr b <> ")"
  OMax a b -> "max(" <> prettyExpr a <> "," <> prettyExpr b <> ")"

prettyCPD :: Problem -> Text
prettyCPD (Problem sensors actuators objectives constraints) =
  [text|
  Actuators:
    $a
  Sensors:
    $s
  Objectives :
    $objs
  Constraints:
    $csts
  |]
  where
    s = mconcat . intersperse "\n" $ Map.toList sensors <&> \(sid, sensor) -> "- " <> sensorID sid <> " : " <> show sensor
    a = mconcat . intersperse "\n" $ Map.toList actuators <&> \(aid, actuator) -> "- " <> actuatorID aid <> " : " <> show actuator
    objs = mconcat . intersperse "\n" $ objectives <&> \(w, o) -> "- weight " <> show (unrefine w) <> ": " <> prettyExpr o
    csts = mconcat . intersperse "\n" $ constraints <&> \(w, c) -> "- threshold " <> show w <> ": " <> prettyExpr c

--- Useful newtypes
newtype OExprSum = OExprSum {getOExprSum :: OExpr}

instance Semigroup OExprSum where
  (coerce -> x) <> (coerce -> y) = coerce (x \+ y)

instance Monoid OExprSum where
  mempty = coerce $ OScalar 0
