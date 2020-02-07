{-# LANGUAGE DerivingVia #-}
{-# OPTIONS_GHC -fno-warn-orphans #-}

-- |
-- Module      : NRM.Types.Controller
-- Copyright   : (c) UChicago Argonne, 2019
-- License     : BSD3
-- Maintainer  : fre@freux.fr
module NRM.Types.Controller
  ( Controller (..),
    Input (..),
    Decision (..),
    Learn (..),
    LearnState,
    LearnConfig,
    LagrangeMultiplier (..),
    initialController,
    enqueueAll,
  )
where

import CPD.Core
import CPD.Integrated as C
import CPD.Values as V
import Data.Aeson as A hiding ((.=))
import Data.JSON.Schema
import Data.MessagePack
import Dhall hiding (field)
import HBandit.BwCR as BwCR
import HBandit.Class
import HBandit.Exp3 as Exp3
import HBandit.Types
import LMap.Map as LMap
import NRM.Classes.Messaging
import NRM.Orphans.NonEmpty ()
import NRM.Orphans.ZeroOne ()
import NRM.Types.MemBuffer as MemBuffer
import NRM.Types.Units
import Protolude hiding (Map)
import Refined
import Refined.Unsafe

data Input
  = -- | A sensor measurement event
    Event Time [V.Measurement]
  | -- | A non-event
    NoEvent Time
  | -- | Events
    Reconfigure Time
  deriving (Show)

data Decision = DoNothing | Decision [V.Action] deriving (Show)

data Learn a b
  = LagrangeConstraints a
  | KnapsackConstraints b
  deriving (Eq, Show, Generic, MessagePack, Interpret, Inject)

type LearnState = Learn (Exp3 [V.Action]) (BwCR [V.Action] [ZeroOne Double])

newtype LagrangeMultiplier = LagrangeMultiplier Double
  deriving (JSONSchema, A.ToJSON, A.FromJSON) via Double
  deriving (Eq, Show, Generic, MessagePack, Interpret, Inject)

type LearnConfig = Learn LagrangeMultiplier BwCR.T

data Controller
  = Controller
      { integrator :: C.Integrator,
        bandit :: Maybe (Learn (Exp3 [V.Action]) (BwCR [V.Action] [ZeroOne Double])),
        bufferedMeasurements :: Maybe (Map SensorID Double),
        referenceMeasurements :: Map SensorID (MemBuffer Double),
        referenceMeasurementCounter :: Refined NonNegative Int
      }
  deriving (JSONSchema, A.ToJSON, A.FromJSON) via GenericJSON Controller
  deriving (Show, Generic, MessagePack, Interpret, Inject)

enqueueAll :: (Ord k) => Map k a -> Map k (MemBuffer a) -> Map k (MemBuffer a)
enqueueAll m mm =
  mapWithKey
    ( \k abuffer -> lookup k m & \case
        Nothing -> abuffer
        Just a -> enqueue a abuffer
    )
    mm

initialController ::
  Time ->
  Time ->
  [SensorID] ->
  Controller
initialController time minTime sensorIDs = Controller
  { integrator = initIntegrator time minTime sensorIDs,
    bandit = Nothing,
    bufferedMeasurements = Nothing,
    referenceMeasurements = LMap.fromList $ sensorIDs <&> (,MemBuffer.empty),
    referenceMeasurementCounter = unsafeRefine 0
  }

-- Instances to serialize the bandit state.

deriving via (GenericJSON LearnConfig) instance JSONSchema LearnConfig

deriving via (GenericJSON LearnConfig) instance A.ToJSON LearnConfig

deriving via (GenericJSON LearnConfig) instance A.FromJSON LearnConfig

deriving via (GenericJSON LearnState) instance JSONSchema LearnState

deriving via (GenericJSON LearnState) instance A.ToJSON LearnState

deriving via (GenericJSON LearnState) instance A.FromJSON LearnState

deriving via (GenericJSON (BwCR.Weight [V.Action])) instance JSONSchema (BwCR.Weight [V.Action])

deriving via (GenericJSON (BwCR.Weight [V.Action])) instance A.ToJSON (BwCR.Weight [V.Action])

deriving via (GenericJSON (BwCR.Weight [V.Action])) instance A.FromJSON (BwCR.Weight [V.Action])

deriving instance Show (BwCR.Weight [V.Action])

deriving instance MessagePack (BwCR.Weight [V.Action])

deriving instance Interpret (BwCR.Weight [V.Action])

deriving instance Inject (BwCR.Weight [V.Action])

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

deriving via (GenericJSON (BwCR [V.Action] [ZeroOne Double])) instance JSONSchema (BwCR [V.Action] [ZeroOne Double])

deriving via (GenericJSON (BwCR [V.Action] [ZeroOne Double])) instance A.ToJSON (BwCR [V.Action] [ZeroOne Double])

deriving via (GenericJSON (BwCR [V.Action] [ZeroOne Double])) instance A.FromJSON (BwCR [V.Action] [ZeroOne Double])

deriving instance Show (BwCR [V.Action] [ZeroOne Double])

deriving instance MessagePack (BwCR [V.Action] [ZeroOne Double])

deriving instance Interpret (BwCR [V.Action] [ZeroOne Double])

deriving instance Inject (BwCR [V.Action] [ZeroOne Double])

deriving via (GenericJSON (BwCRHyper [V.Action] [ZeroOne Double])) instance JSONSchema (BwCRHyper [V.Action] [ZeroOne Double])

deriving via (GenericJSON (BwCRHyper [V.Action] [ZeroOne Double])) instance A.ToJSON (BwCRHyper [V.Action] [ZeroOne Double])

deriving via (GenericJSON (BwCRHyper [V.Action] [ZeroOne Double])) instance A.FromJSON (BwCRHyper [V.Action] [ZeroOne Double])

deriving instance Show (BwCRHyper [V.Action] [ZeroOne Double])

deriving instance MessagePack (BwCRHyper [V.Action] [ZeroOne Double])

deriving instance Interpret (BwCRHyper [V.Action] [ZeroOne Double])

deriving instance Inject (BwCRHyper [V.Action] [ZeroOne Double])

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

deriving via (GenericJSON BwCR.T) instance JSONSchema BwCR.T

deriving via (GenericJSON BwCR.T) instance A.ToJSON BwCR.T

deriving via (GenericJSON BwCR.T) instance A.FromJSON BwCR.T

deriving instance Show BwCR.T

deriving instance MessagePack BwCR.T

deriving instance Interpret BwCR.T

deriving instance Inject BwCR.T

deriving via (GenericJSON Integrator) instance JSONSchema Integrator

deriving via (GenericJSON Integrator) instance A.ToJSON Integrator

deriving via (GenericJSON Integrator) instance A.FromJSON Integrator

deriving instance Show Integrator

deriving instance MessagePack Integrator

deriving instance Interpret Integrator

deriving instance Inject Integrator

deriving via (GenericJSON (ScreeningBwCR [Action] [ZeroOne Double])) instance JSONSchema (ScreeningBwCR [Action] [ZeroOne Double])

deriving via (GenericJSON (ScreeningBwCR [Action] [ZeroOne Double])) instance A.ToJSON (ScreeningBwCR [Action] [ZeroOne Double])

deriving via (GenericJSON (ScreeningBwCR [Action] [ZeroOne Double])) instance A.FromJSON (ScreeningBwCR [Action] [ZeroOne Double])

deriving instance Show (ScreeningBwCR [Action] [ZeroOne Double])

deriving instance MessagePack (ScreeningBwCR [Action] [ZeroOne Double])

deriving instance Interpret (ScreeningBwCR [Action] [ZeroOne Double])

deriving instance Inject (ScreeningBwCR [Action] [ZeroOne Double])

deriving via (GenericJSON (UCBBwCR [Action] [ZeroOne Double])) instance JSONSchema (UCBBwCR [Action] [ZeroOne Double])

deriving via (GenericJSON (UCBBwCR [Action] [ZeroOne Double])) instance A.ToJSON (UCBBwCR [Action] [ZeroOne Double])

deriving via (GenericJSON (UCBBwCR [Action] [ZeroOne Double])) instance A.FromJSON (UCBBwCR [Action] [ZeroOne Double])

deriving instance Show (UCBBwCR [Action] [ZeroOne Double])

deriving instance MessagePack (UCBBwCR [Action] [ZeroOne Double])

deriving instance Interpret (UCBBwCR [Action] [ZeroOne Double])

deriving instance Inject (UCBBwCR [Action] [ZeroOne Double])

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
