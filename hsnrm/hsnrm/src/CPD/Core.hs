{-# LANGUAGE DerivingVia #-}
{-# LANGUAGE QuasiQuotes #-}
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

import Bandit.Types
import Control.Arrow
import Control.Lens
import qualified Data.Aeson as A
import Data.Coerce
import Data.Data
import Data.Generics.Labels ()
import Data.JSON.Schema
import Data.Map as M
import Data.MessagePack
import Dhall
import NRM.Classes.Messaging
import NRM.Orphans.UUID ()
import NRM.Orphans.ZeroOne ()
import qualified NRM.Types.Units as Units
import NeatInterpolation
import Numeric.Interval as I hiding (elem)
import Protolude hiding (Map)
import Refined

-- METADATA
newtype Discrete = DiscreteDouble {getDiscrete :: Double}
  deriving
    ( Show,
      Eq,
      Ord,
      Generic,
      MessagePack
    )
  deriving (JSONSchema, A.ToJSON, A.FromJSON, FromDhall, ToDhall) via Double

data Problem
  = Problem
      { sensors :: Map SensorID Sensor,
        actuators :: Map ActuatorID Actuator,
        objectives :: [(ZeroOne Double, OExpr)],
        constraints :: [(Double, OExpr)]
      }
  deriving (Show, Generic, MessagePack, FromDhall, ToDhall)
  deriving (JSONSchema, A.ToJSON, A.FromJSON) via GenericJSON Problem

emptyProblem :: Problem
emptyProblem = Problem M.empty M.empty [] []

-- | A sensor's metadata.
data Sensor = Sensor {range :: Interval Double, maxFrequency :: Units.Frequency}
  deriving (Show, Generic, MessagePack, FromDhall, ToDhall)
  deriving (JSONSchema, A.ToJSON, A.FromJSON) via GenericJSON Sensor

deriving instance MessagePack (Interval Double)

deriving instance FromDhall (Interval Double)

deriving instance ToDhall (Interval Double)

deriving via GenericJSON (Interval Double) instance JSONSchema (Interval Double)

deriving via GenericJSON (Interval Double) instance A.ToJSON (Interval Double)

deriving via GenericJSON (Interval Double) instance A.FromJSON (Interval Double)

newtype Admissible = Admissible {admissibleValues :: [Discrete]}
  deriving (Show, Generic, MessagePack, FromDhall, ToDhall)
  deriving (JSONSchema, A.ToJSON, A.FromJSON) via GenericJSON Admissible

-------- SENSORS

-- | An unique identifier for a sensor.
newtype SensorID = SensorID {sensorID :: Text}
  deriving
    ( Ord,
      Eq,
      Show,
      Generic,
      MessagePack,
      A.ToJSONKey,
      A.FromJSONKey,
      FromDhall,
      ToDhall,
      Data
    )
  deriving (IsString, JSONSchema, A.ToJSON, A.FromJSON) via Text

------- ACTUATORS
class CPDLActuator a where
  toActuator :: a -> Actuator

-- | An unique identifier for an actuator.
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
      IsString,
      FromDhall,
      ToDhall
    )
    via Text

-- | An actuator's metadata.
newtype Actuator = Actuator {actions :: [Discrete]}
  deriving (JSONSchema, A.ToJSON, A.FromJSON) via GenericJSON Actuator
  deriving (Show, Generic, MessagePack)
  deriving (FromDhall, ToDhall) via [Discrete]

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
  deriving (Show, Eq, Generic, MessagePack, FromDhall, ToDhall, Data)

deriving instance Plated OExpr

rules :: OExpr -> Maybe OExpr
rules = \case
  (OScalar 0 `OAdd` e) -> Just e
  (e `OAdd` OScalar 0) -> Just e
  (e `OSub` OScalar 0) -> Just e
  (OScalar 1 `OMul` e) -> Just e
  (e `OMul` OScalar 1) -> Just e
  (e `ODiv` OScalar 1) -> Just e
  _ -> Nothing

simplify :: OExpr -> OExpr
simplify = rewrite rules

prettyExpr :: OExpr -> Text
prettyExpr = \case
  OValue (SensorID id) -> [text| "SENSOR: $id" |]
  OReference (SensorID id) -> [text| "REF: $id" |]
  OScalar d -> show d
  OAdd (prettyExpr -> a) (prettyExpr -> b) -> [text|$a+$b|]
  OSub (prettyExpr -> a) (prettyExpr -> b) -> [text|$a-$b|]
  OMul (OScalar (show -> d)) (prettyExpr -> b) -> [text|$d*($b)|]
  OMul (prettyExpr -> a) (prettyExpr -> b) -> [text|($a)*($b)|]
  ODiv (prettyExpr -> a) (prettyExpr -> b) -> [text|($a)/($b)|]
  OMin (prettyExpr -> a) (prettyExpr -> b) -> [text|min($a,$b)|]
  OMax (prettyExpr -> a) (prettyExpr -> b) -> [text|max($a,$b)|]

showExpr :: OExpr -> Text
showExpr = prettyExpr . simplify

prettyCPD :: Problem -> Text
prettyCPD p =
  [text| Actuators:     $a
         Sensors:       $s
         Objectives:    $objs
         Constraints:   $csts |]
  where
    s = mcUnlines $ p ^@.. #sensors . itraversed <&> ((sensorID *** show) >>> desc)
    a = mcUnlines $ p ^@.. #actuators . itraversed <&> ((actuatorID *** show) >>> desc)
    objs = mcUnlines $ p ^. #objectives <&> (show . unrefine *** showExpr >>> desc)
    csts = mcUnlines $ p ^. #constraints <&> (show *** showExpr >>> desc)
    desc :: (Text, Text) -> Text
    desc (d, e) = "- " <> d <> " : " <> e
    mcUnlines :: [Text] -> Text
    mcUnlines = mconcat . intersperse "\n"

--- Useful newtypes
newtype OExprSum = OExprSum {getOExprSum :: OExpr}

instance Semigroup OExprSum where
  (coerce -> x) <> (coerce -> y) = coerce (x \+ y)

instance Monoid OExprSum where
  mempty = coerce $ OScalar 0
