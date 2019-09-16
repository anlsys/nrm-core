{-# LANGUAGE DerivingVia #-}

{-|
Module      : CPD.Integrated
Copyright   : (c) UChicago Argonne, 2019
License     : BSD3
Maintainer  : fre@freux.fr
-}
module CPD.Integrated
  ( Integrator (..)
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
      , measured :: Map SensorID [(Time, Double)]
      }

data Integrated
  = Integrated
      { integrated :: Map SensorID Double
      }

initIntegrator :: Problem -> Integrator
initIntegrator = undefined

data IntegratorAction = DoNothing | TriggerStep

updateIntegrator :: (MonadState Integrator m) => Measurement -> m IntegratorAction
updateIntegrator = undefined
