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
    initialController,
  )
where

import Bandit.Exp3
import CPD.Core as C
import CPD.Integrated as C
import CPD.Values as V
import Data.Aeson as A hiding ((.=))
import Data.Data
import Data.JSON.Schema
import Data.MessagePack
import Dhall hiding (field)
import NRM.Classes.Messaging
import NRM.Orphans.NonEmpty ()
import NRM.Types.Units
import Protolude

data Input
  = -- | A sensor measurement event
    Event Time [V.Measurement]
  | -- | A non-event
    NoEvent Time
  | -- | Events
    Reconfigure Time C.Problem

data Decision = DoNothing | Decision [V.Action] deriving (Show)

data Controller
  = Controller
      { integratedProblem :: Maybe C.IntegratedProblem,
        integrator :: C.Integrator,
        bandit :: Maybe (Exp3 [V.Action])
      }
  deriving (JSONSchema, A.ToJSON, A.FromJSON) via GenericJSON Controller
  deriving (Show, Generic, Data, MessagePack, Interpret, Inject)

initialController :: Time -> Time -> Controller
initialController time minTime = Controller
  { integratedProblem = Nothing,
    integrator = initIntegrator time minTime,
    bandit = Nothing
  }

-- Bunch of instances to serialize the bandit state..
--
deriving via (GenericJSON (Weight [V.Action])) instance JSONSchema (Weight [V.Action])

deriving via (GenericJSON (Weight [V.Action])) instance A.ToJSON (Weight [V.Action])

deriving via (GenericJSON (Weight [V.Action])) instance A.FromJSON (Weight [V.Action])

deriving instance Show (Weight [V.Action])

deriving instance Data (Weight [V.Action])

deriving instance MessagePack (Weight [V.Action])

deriving instance Interpret (Weight [V.Action])

deriving instance Inject (Weight [V.Action])

deriving via (GenericJSON (Exp3 [V.Action])) instance JSONSchema (Exp3 [V.Action])

deriving via (GenericJSON (Exp3 [V.Action])) instance A.ToJSON (Exp3 [V.Action])

deriving via (GenericJSON (Exp3 [V.Action])) instance A.FromJSON (Exp3 [V.Action])

deriving instance Show (Exp3 [V.Action])

deriving instance Data (Exp3 [V.Action])

deriving instance MessagePack (Exp3 [V.Action])

deriving instance Interpret (Exp3 [V.Action])

deriving instance Inject (Exp3 [V.Action])

deriving via (GenericJSON Probability) instance JSONSchema Probability

deriving via (GenericJSON Probability) instance A.ToJSON Probability

deriving via (GenericJSON Probability) instance A.FromJSON Probability

deriving instance Show Probability

deriving instance Data Probability

deriving instance MessagePack Probability

deriving instance Interpret Probability

deriving instance Inject Probability

deriving via (GenericJSON CumulativeLoss) instance JSONSchema CumulativeLoss

deriving via (GenericJSON CumulativeLoss) instance A.ToJSON CumulativeLoss

deriving via (GenericJSON CumulativeLoss) instance A.FromJSON CumulativeLoss

deriving instance Show CumulativeLoss

deriving instance Data CumulativeLoss

deriving instance MessagePack CumulativeLoss

deriving instance Interpret CumulativeLoss

deriving instance Inject CumulativeLoss

deriving via (GenericJSON Integrator) instance JSONSchema Integrator

deriving via (GenericJSON Integrator) instance A.ToJSON Integrator

deriving via (GenericJSON Integrator) instance A.FromJSON Integrator

deriving instance Show Integrator

deriving instance Data Integrator

deriving instance MessagePack Integrator

deriving instance Interpret Integrator

deriving instance Inject Integrator
