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
  , initIntegrator
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
integrateProblem = undefined

initIntegrator :: IntegratedProblem -> Integrator
initIntegrator = undefined

step :: (MonadState Integrator m) => Measurements -> m IntegratorAction
step = undefined
