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

    -- * Objective
    Objective,
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
    prettyObj,
  )
where

import qualified Data.Aeson as A
import Data.Data
import Data.JSON.Schema
import qualified Data.Map as DM
import Data.MessagePack
import Data.String (IsString (..))
import qualified Dhall as D
import NRM.Classes.Messaging
import NRM.Orphans.UUID ()
import qualified NRM.Types.Units as Units
import NeatInterpolation
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
      D.Interpret,
      D.Inject
    )
  deriving (JSONSchema, A.ToJSON, A.FromJSON) via GenericJSON Discrete

data Problem
  = Problem
      { sensors :: Map SensorID Sensor,
        actuators :: Map ActuatorID Actuator,
        objective :: Objective
      }
  deriving (Show, Generic, Data, MessagePack, D.Interpret, D.Inject)
  deriving (JSONSchema, A.ToJSON, A.FromJSON) via GenericJSON Problem

emptyProblem :: Problem
emptyProblem = Problem DM.empty DM.empty emptyObjective

data Sensor = Sensor {range :: Interval Double, maxFrequency :: Units.Frequency}
  deriving (Show, Generic, Data, MessagePack, D.Interpret, D.Inject)
  deriving (JSONSchema, A.ToJSON, A.FromJSON) via GenericJSON Sensor

deriving instance MessagePack (Interval Double)

deriving instance D.Interpret (Interval Double)

deriving instance D.Inject (Interval Double)

deriving via GenericJSON (Interval Double) instance JSONSchema (Interval Double)

deriving via GenericJSON (Interval Double) instance A.ToJSON (Interval Double)

deriving via GenericJSON (Interval Double) instance A.FromJSON (Interval Double)

newtype Admissible = Admissible {admissibleValues :: [Discrete]}
  deriving (Show, Generic, Data, MessagePack, D.Interpret, D.Inject)
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
      D.Interpret,
      D.Inject
    )
  deriving (IsString) via Text

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
      D.Interpret,
      D.Inject
    )
  deriving (JSONSchema, A.ToJSON, A.FromJSON) via GenericJSON ActuatorID

instance IsString ActuatorID where
  fromString x = fromMaybe (panic "couldn't decode ActuatorID in FromString instance") (A.decode $ toS x)

newtype Actuator = Actuator {actions :: [Discrete]}
  deriving (JSONSchema, A.ToJSON, A.FromJSON) via GenericJSON Actuator
  deriving (Show, Generic, Data, MessagePack)
  deriving (D.Interpret, D.Inject) via [Discrete]

------- OBJECTIVE
type Objective = Maybe OExpr

emptyObjective :: Objective
emptyObjective = Nothing

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
  deriving (JSONSchema, A.ToJSON, A.FromJSON) via GenericJSON OExpr
  deriving (Show, Eq, Generic, Data, MessagePack, D.Interpret, D.Inject)

prettyOExpr = \case
  OValue (SensorID id) -> id
  OScalar d -> show d
  OAdd a b -> prettyOExpr a <> "+" <> prettyOExpr b
  OSub a b -> "(" <> prettyOExpr a <> "-" <> prettyOExpr b <> ")"
  OMul (OScalar d) b -> show d <> "*(" <> prettyOExpr b <> ")"
  OMul a b -> "(" <> prettyOExpr a <> ")*(" <> prettyOExpr b <> ")"
  ODiv a b -> "(" <> prettyOExpr a <> ")/(" <> prettyOExpr b <> ")"

prettyObj :: Objective -> Text
prettyObj Nothing = "No objective"
prettyObj (Just o) = prettyOExpr o

prettyCPD :: Problem -> Text
prettyCPD (Problem sensors actuators objective) =
  [text|
  Sensors : $s
  Actuators : $a
  Objective : $o
  |]
  where
    s = show sensors
    a = show actuators
    o = prettyObj objective
