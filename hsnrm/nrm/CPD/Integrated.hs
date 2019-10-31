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
    calculate,
    Calculate (..),
  )
where

import CPD.Core
import Data.Map as DM
import CPD.Utils
import qualified Data.Aeson as A
import Data.Data
import Data.JSON.Schema
import Data.MessagePack
import Dhall
import NRM.Classes.Messaging
import NRM.Types.Units
import Numeric.Interval as I hiding (elem)
import Protolude

data IntegratedProblem
  = IntegratedProblem
      { isensors :: Map SensorID (Interval Double),
        iactuators :: Map ActuatorID Actuator,
        iobjective :: Objective,
        iobjRange :: I.Interval Double
      }
  deriving (Show, Generic, Data, MessagePack, Interpret, Inject)
  deriving
    (JSONSchema, A.ToJSON, A.FromJSON)
    via GenericJSON IntegratedProblem

data Integrator
  = Integrator
      { tLast :: Time,
        maximumControlFrequency :: Frequency,
        measured :: Map SensorID [(Time, Double)]
      }
  deriving (Generic)

data IntegratorAction = IntegratorPasses | TriggerStep Integrator

integrateProblem :: Problem -> Maybe IntegratedProblem
integrateProblem p =
  IntegratedProblem
    <$> (Just ranges)
    <*> (Just $ actuators p)
    <*> (Just $ objective p)
    <*> (evalRange ranges =<< objective p)
  where
    ranges = sensors p <&> range

initIntegrator ::
  Time ->
  Frequency ->
  Integrator
initIntegrator t f = Integrator
  { tLast = t,
    maximumControlFrequency = f,
    measured = DM.empty
  }

data Calculate
  = Calculate
      { remains :: Map SensorID [(Time, Double)],
        measurements :: Map SensorID Double
      }

calculate ::
  Time ->
  Integrator ->
  IntegratedProblem ->
  Maybe Calculate
calculate time i ipb = Nothing
