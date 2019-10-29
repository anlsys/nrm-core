{-# LANGUAGE DerivingVia #-}

-- |
-- Module      : CPD.Integrated
-- Copyright   : (c) UChicago Argonne, 2019
-- License     : BSD3
-- Maintainer  : fre@freux.fr
module CPD.Integrated
  ( Integrator (..),
    IntegratedProblem (..),
    IntegratorAction (..),
    integrateProblem,
    initIntegrator,
    stepIntegrator,
  )
where

import CPD.Core
import CPD.Values
import qualified Data.Aeson as A
import Data.Data
import Data.JSON.Schema
import Data.MessagePack
import Dhall
import NRM.Classes.Messaging
import NRM.Types.Units
import Protolude

data IntegratedProblem = IntegratedProblem {}
  deriving (Show, Generic, Data, MessagePack, Interpret, Inject)
  deriving (JSONSchema, A.ToJSON, A.FromJSON) via GenericJSON IntegratedProblem

data Integrator
  = Integrator
      { tLast :: Time,
        minPeriod :: Time,
        measured :: Map SensorID [(Time, Double)]
      }
  deriving (Generic)

data IntegratorAction = IntegratorPasses | TriggerStep Integrator

integrateProblem :: Problem -> IntegratedProblem
integrateProblem = panic "integrateProblem not implemented"

initIntegrator ::
  Time ->
  IntegratedProblem ->
  Integrator
initIntegrator = panic "initIntegrator not implemented"

stepIntegrator ::
  (MonadState Integrator m) =>
  [Measurement] ->
  m IntegratorAction
stepIntegrator _measurements = panic "stepIntegrator not implemented"
