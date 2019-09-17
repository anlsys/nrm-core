{-# LANGUAGE DerivingVia #-}

{-|
Module      : CPD.Integrated
Copyright   : (c) UChicago Argonne, 2019
License     : BSD3
Maintainer  : fre@freux.fr
-}
module CPD.Integrated
  ( Integrator (..)
  , IntegratedProblem (..)
  , IntegratorAction (..)
  , Integrated (..)
  , integrateProblem
  , initIntegrator
  , stepIntegrator
  )
where

import CPD.Core
import CPD.Values
import NRM.Types.Units
import Protolude

data IntegratedProblem = IntegratedProblem {}

data Integrator
  = Integrator
      { tLast :: Time
      , minPeriod :: Time
      , measured :: Map SensorID [(Time, Double)]
      }
  deriving (Generic)

data Integrated
  = Integrated
      { integrated :: Map SensorID Double
      }
  deriving (Generic)

data IntegratorAction = IntegratorPasses | TriggerStep Integrated

integrateProblem :: Problem -> IntegratedProblem
integrateProblem = panic "integrateProblem not implemented"

initIntegrator :: IntegratedProblem -> Integrator
initIntegrator = panic "initIntegrator not implemented"

stepIntegrator :: (MonadState Integrator m) => Measurements -> m IntegratorAction
stepIntegrator _measurements = panic "stepIntegrator not implemented"
