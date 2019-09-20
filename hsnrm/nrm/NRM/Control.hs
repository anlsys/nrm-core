{-# LANGUAGE DerivingVia #-}

{-# OPTIONS_GHC -fno-warn-orphans #-}

{-|
Module      : NRM.Control
Copyright   : (c) UChicago Argonne, 2019
License     : BSD3
Maintainer  : fre@freux.fr
-}
module NRM.Control
  ( Input (..)
  , Decision (..)
  , Controller (..)
  , BanditActions (..)
  , control
  , initialController
  )
where

import Bandit.Exp3
import CPD.Core as C
import CPD.Integrated as C
import CPD.Values as V
import Control.Lens
import Data.Aeson as A hiding ((.=))
import Data.Data
import Data.Generics.Product
import Data.JSON.Schema
import Data.MessagePack
import Dhall hiding (field)
import NRM.Classes.Messaging
import NRM.Orphans.NonEmpty ()
import NRM.Types.Units
import Protolude

data Input
  = -- | A sensor measurement event
    Event V.Measurements
  | -- | A sensor measurement event
    Time Time
  | -- | Events
    Reconfigure C.Problem

data Decision = DoNothing | Decision V.Actions

newtype BanditActions = BanditActions [V.Actions]

data Controller
  = Controller
      { cpd :: C.Problem
      , integrator :: C.Integrator
      , bandit :: Maybe (Exp3 SensorID)
      }
  deriving (JSONSchema, A.ToJSON, A.FromJSON) via GenericJSON Controller
  deriving (Show, Generic, Data, MessagePack, Interpret, Inject)

initialController :: Controller
initialController = Controller {cpd = C.emptyProblem, bandit = Nothing}

control :: (MonadState Controller m) => Input -> m Decision
control (Reconfigure c) = do
  field @"cpd" .= c
  {-field @"bandit" .= init [ ]-}
  return DoNothing

-- Bunch of instances to serialize the bandit state..
--
deriving via (GenericJSON (Weight SensorID)) instance JSONSchema (Weight SensorID)

deriving via (GenericJSON (Weight SensorID)) instance A.ToJSON (Weight SensorID)

deriving via (GenericJSON (Weight SensorID)) instance A.FromJSON (Weight SensorID)

deriving instance Show (Weight SensorID)

deriving instance Data (Weight SensorID)

deriving instance MessagePack (Weight SensorID)

deriving instance Interpret (Weight SensorID)

deriving instance Inject (Weight SensorID)

deriving via (GenericJSON (Exp3 SensorID)) instance JSONSchema (Exp3 SensorID)

deriving via (GenericJSON (Exp3 SensorID)) instance A.ToJSON (Exp3 SensorID)

deriving via (GenericJSON (Exp3 SensorID)) instance A.FromJSON (Exp3 SensorID)

deriving instance Show (Exp3 SensorID)

deriving instance Data (Exp3 SensorID)

deriving instance MessagePack (Exp3 SensorID)

deriving instance Interpret (Exp3 SensorID)

deriving instance Inject (Exp3 SensorID)

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
