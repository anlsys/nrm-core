{-# LANGUAGE DerivingVia #-}
{-# OPTIONS_GHC -fno-warn-orphans #-}
{-# OPTIONS_GHC -fno-warn-partial-fields #-}

-- |
-- Module      : NRM.Types.Controller
-- Copyright   : (c) UChicago Argonne, 2019
-- License     : BSD3
-- Maintainer  : fre@freux.fr
module NRM.Types.Controller
  ( Controller (..),
    Input (..),
    Decision (..),
    ObjectiveValue (..),
    ConstraintValue (..),
    DecisionMetadata (..),
    Learn (..),
    LearnState,
    LearnConfig,
    LagrangeMultiplier (..),
    CtxCfg (..),
    Hint (..),
    Seed (..),
    Uniform (..),
    UniformCfg (..),
    Armstat (..),
    initialController,
    enqueueAll,
  )
where

import Bandit.Class
import Bandit.Exp3 as Exp3
import Bandit.Exp4R as Exp4R
import Bandit.Types
import CPD.Core
import CPD.Integrated as C
import CPD.Values as V
import Data.Aeson as A hiding ((.=))
import Data.JSON.Schema
import Data.Map.Merge.Lazy
import Data.MessagePack
import Dhall hiding (field)
import LMap.Map as LM
import NRM.Classes.Messaging
import NRM.Orphans.NonEmpty ()
import NRM.Orphans.ZeroOne ()
import NRM.Types.MemBuffer as MemBuffer
import NRM.Types.Units
import Protolude hiding (Map)
import Refined hiding (NonEmpty)
import Refined.Unsafe

type LearnState =
  Learn
    (Exp3 [V.Action])
    (Uniform [V.Action])
    (Exp4R () [V.Action] (ObliviousRep [V.Action]))

type LearnConfig = Learn LagrangeMultiplier UniformCfg CtxCfg

data Input
  = -- | A sensor measurement event
    Event Time [V.Measurement]
  | -- | A non-event
    NoEvent Time
  | -- | Events
    Reconfigure Time
  deriving (Show)

newtype ConstraintValue = ConstraintValue {fromConstraintValue :: Double}
  deriving (Show, Generic, MessagePack)
  deriving (JSONSchema, ToJSON, FromJSON) via GenericJSON ConstraintValue

newtype ObjectiveValue = ObjectiveValue {fromObjectiveValue :: Double}
  deriving (Show, Generic, MessagePack)
  deriving (JSONSchema, ToJSON, FromJSON) via GenericJSON ObjectiveValue

data DecisionMetadata
  = InitialDecision
  | ReferenceMeasurementDecision
  | InnerDecision
      { constraints :: [ConstraintValue],
        objectives :: [ObjectiveValue],
        loss :: ZeroOne Double,
        reportEvaluatedObjectives :: [(ZeroOne Double, Maybe Double, Maybe (Interval Double))],
        reportNormalizedObjectives :: [(ZeroOne Double, ZeroOne Double)],
        reportEvaluatedConstraints :: [(Double, Double, Interval Double)]
      }
  deriving (Show, Generic, MessagePack)
  deriving (JSONSchema, ToJSON, FromJSON) via GenericJSON DecisionMetadata

data Decision = DoNothing | Decision [V.Action] DecisionMetadata deriving (Show)

data Learn a c d
  = Lagrange {lagrange :: a}
  | Random {random :: c}
  | Contextual {contextual :: d}
  deriving (Eq, Show, Generic, MessagePack, Interpret, Inject)

newtype LagrangeMultiplier = LagrangeMultiplier Double
  deriving (JSONSchema, A.ToJSON, A.FromJSON, Interpret, Inject) via Double
  deriving (Eq, Show, Generic, MessagePack)

newtype Seed = Seed Int
  deriving (JSONSchema, A.ToJSON, A.FromJSON, Interpret, Inject) via Int
  deriving (Eq, Show, Generic, MessagePack)

newtype CtxCfg = CtxCfg {horizon :: Int}
  deriving (Eq, Show, Generic, MessagePack, Interpret, Inject)
  deriving (JSONSchema, ToJSON, FromJSON) via GenericJSON CtxCfg

data Hint = Full | Only {only :: NonEmpty [Action]}
  deriving (Eq, Show, Generic, MessagePack, Interpret, Inject)
  deriving (JSONSchema, ToJSON, FromJSON) via GenericJSON Hint

data Controller
  = Controller
      { integrator :: C.Integrator,
        bandit :: Maybe LearnState,
        lastA :: Maybe [V.Action],
        armstats :: Map [V.Action] Armstat,
        bufferedMeasurements :: Maybe (Map SensorID Double),
        referenceMeasurements :: Map SensorID (MemBuffer Double),
        referenceMeasurementCounter :: Refined NonNegative Int
      }
  deriving (JSONSchema, A.ToJSON, A.FromJSON) via GenericJSON Controller
  deriving (Show, Generic, MessagePack, Interpret, Inject)

data Armstat
  = Armstat
      Int -- pulls
      Double -- avgLoss
      [Double] --avgObj
      [Double] --avgCst
      (Map SensorID Double) --avgMeasurements
      (Map SensorID Double) --avgReferences
  deriving (JSONSchema, A.ToJSON, A.FromJSON) via GenericJSON Armstat
  deriving (Show, Generic, MessagePack, Interpret, Inject)

enqueueAll :: (Ord k) => Map k a -> Map k (MemBuffer a) -> Map k (MemBuffer a)
enqueueAll
  (toDataMap -> m)
  (toDataMap -> mapBuffers) =
    fromDataMap $
      merge
        (mapMissing $ \_ a -> MemBuffer.singleton a)
        preserveMissing
        (zipWithMatched $ \_ a abuffer -> enqueue a abuffer)
        m
        mapBuffers

initialController ::
  Time ->
  Time ->
  [SensorID] ->
  Controller
initialController time minTime sensorIDs = Controller
  { integrator = initIntegrator time minTime sensorIDs,
    lastA = Nothing,
    bandit = Nothing,
    armstats = LM.empty,
    bufferedMeasurements = Nothing,
    referenceMeasurements = LM.fromList $ sensorIDs <&> (,MemBuffer.empty),
    referenceMeasurementCounter = unsafeRefine 0
  }

newtype UniformCfg = UniformCfg {seed :: Maybe Seed}
  deriving (Show, Eq, Generic) via (Maybe Seed)

-- Uniform controller

newtype Uniform a = Uniform [a]
  deriving (Show, Generic)

deriving instance MessagePack UniformCfg

deriving via (Maybe Seed) instance Interpret UniformCfg

deriving via (Maybe Seed) instance Inject UniformCfg

deriving via (GenericJSON UniformCfg) instance JSONSchema UniformCfg

deriving via (GenericJSON UniformCfg) instance A.ToJSON UniformCfg

deriving via (GenericJSON UniformCfg) instance A.FromJSON UniformCfg

deriving instance MessagePack (Uniform [V.Action])

deriving instance Interpret (Uniform [V.Action])

deriving instance Inject (Uniform [V.Action])

deriving via (GenericJSON (Uniform [V.Action])) instance JSONSchema (Uniform [V.Action])

deriving via (GenericJSON (Uniform [V.Action])) instance A.ToJSON (Uniform [V.Action])

deriving via (GenericJSON (Uniform [V.Action])) instance A.FromJSON (Uniform [V.Action])

-- Instances to serialize the bandit state:Exp4R
deriving instance Show (LastAction [V.Action])

deriving instance Show (ObliviousRep [V.Action])

deriving instance MessagePack (LastAction [V.Action])

deriving instance MessagePack (ObliviousRep [V.Action])

deriving instance Interpret (LastAction [V.Action])

deriving instance Interpret (ObliviousRep [V.Action])

deriving instance Inject (LastAction [V.Action])

deriving instance Inject (ObliviousRep [V.Action])

deriving via (GenericJSON (LastAction [V.Action])) instance JSONSchema (LastAction [V.Action])

deriving via (GenericJSON (ObliviousRep [V.Action])) instance JSONSchema (ObliviousRep [V.Action])

deriving via (GenericJSON (LastAction [V.Action])) instance A.ToJSON (LastAction [V.Action])

deriving via (GenericJSON (ObliviousRep [V.Action])) instance A.ToJSON (ObliviousRep [V.Action])

deriving via (GenericJSON (LastAction [V.Action])) instance A.FromJSON (LastAction [V.Action])

deriving via (GenericJSON (ObliviousRep [V.Action])) instance A.FromJSON (ObliviousRep [V.Action])

deriving instance Show (Exp4R () [V.Action] (ObliviousRep [V.Action]))

deriving instance MessagePack (Exp4R () [V.Action] (ObliviousRep [V.Action]))

deriving instance Interpret (Exp4R () [V.Action] (ObliviousRep [V.Action]))

deriving instance Inject (Exp4R () [V.Action] (ObliviousRep [V.Action]))

deriving via (GenericJSON (Exp4R () [V.Action] (ObliviousRep [V.Action]))) instance JSONSchema (Exp4R () [V.Action] (ObliviousRep [V.Action]))

deriving via (GenericJSON (Exp4R () [V.Action] (ObliviousRep [V.Action]))) instance A.ToJSON (Exp4R () [V.Action] (ObliviousRep [V.Action]))

deriving via (GenericJSON (Exp4R () [V.Action] (ObliviousRep [V.Action]))) instance A.FromJSON (Exp4R () [V.Action] (ObliviousRep [V.Action]))

-- Instances to serialize the bandit state.

deriving via (GenericJSON LearnConfig) instance JSONSchema LearnConfig

deriving via (GenericJSON LearnConfig) instance A.ToJSON LearnConfig

deriving via (GenericJSON LearnConfig) instance A.FromJSON LearnConfig

deriving via (GenericJSON LearnState) instance JSONSchema LearnState

deriving via (GenericJSON LearnState) instance A.ToJSON LearnState

deriving via (GenericJSON LearnState) instance A.FromJSON LearnState

deriving via (GenericJSON (Exp3.Weight [V.Action])) instance JSONSchema (Exp3.Weight [V.Action])

deriving via (GenericJSON (Exp3.Weight [V.Action])) instance A.ToJSON (Exp3.Weight [V.Action])

deriving via (GenericJSON (Exp3.Weight [V.Action])) instance A.FromJSON (Exp3.Weight [V.Action])

deriving instance Show (Exp3.Weight [V.Action])

deriving instance MessagePack (Exp3.Weight [V.Action])

deriving instance Interpret (Exp3.Weight [V.Action])

deriving instance Inject (Exp3.Weight [V.Action])

deriving via (GenericJSON (Exp3 [V.Action])) instance JSONSchema (Exp3 [V.Action])

deriving via (GenericJSON (Exp3 [V.Action])) instance A.ToJSON (Exp3 [V.Action])

deriving via (GenericJSON (Exp3 [V.Action])) instance A.FromJSON (Exp3 [V.Action])

deriving instance Show (Exp3 [V.Action])

deriving instance MessagePack (Exp3 [V.Action])

deriving instance Interpret (Exp3 [V.Action])

deriving instance Inject (Exp3 [V.Action])

deriving via (GenericJSON Probability) instance JSONSchema Probability

deriving via (GenericJSON Probability) instance A.ToJSON Probability

deriving via (GenericJSON Probability) instance A.FromJSON Probability

deriving instance Show Probability

deriving instance MessagePack Probability

deriving instance Interpret Probability

deriving instance Inject Probability

deriving via (GenericJSON CumulativeLoss) instance JSONSchema CumulativeLoss

deriving via (GenericJSON CumulativeLoss) instance A.ToJSON CumulativeLoss

deriving via (GenericJSON CumulativeLoss) instance A.FromJSON CumulativeLoss

deriving instance Show CumulativeLoss

deriving instance MessagePack CumulativeLoss

deriving instance Interpret CumulativeLoss

deriving instance Inject CumulativeLoss

deriving via (GenericJSON Integrator) instance JSONSchema Integrator

deriving via (GenericJSON Integrator) instance A.ToJSON Integrator

deriving via (GenericJSON Integrator) instance A.FromJSON Integrator

deriving instance Show Integrator

deriving instance MessagePack Integrator

deriving instance Interpret Integrator

deriving instance Inject Integrator

deriving via (GenericJSON (Arms [Action])) instance JSONSchema (Arms [Action])

deriving via (GenericJSON (Arms [Action])) instance A.ToJSON (Arms [Action])

deriving via (GenericJSON (Arms [Action])) instance A.FromJSON (Arms [Action])

deriving instance MessagePack (Arms [Action])

deriving instance Interpret (Arms [Action])

deriving instance Inject (Arms [Action])

deriving via (GenericJSON (Interval [ZeroOne Double])) instance JSONSchema (Interval [ZeroOne Double])

deriving via (GenericJSON (Interval [ZeroOne Double])) instance A.ToJSON (Interval [ZeroOne Double])

deriving via (GenericJSON (Interval [ZeroOne Double])) instance A.FromJSON (Interval [ZeroOne Double])

deriving instance MessagePack (Interval [ZeroOne Double])

deriving instance Interpret (Interval [ZeroOne Double])

deriving instance Inject (Interval [ZeroOne Double])
