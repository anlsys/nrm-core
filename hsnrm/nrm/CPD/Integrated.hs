{-# LANGUAGE DerivingVia #-}

{-|
Module      : CPD.Integrated
Copyright   : (c) UChicago Argonne, 2019
License     : BSD3
Maintainer  : fre@freux.fr
-}
module CPD.Integrated
  (
  )
where

import CPD.Core
import CPD.Values
import Protolude

data Integrator = Integrator {}

initIntegrator :: Problem -> Integrator
initIntegrator = undefined

updateIntegrator :: Integrator -> Measurement
updateIntegrator = undefined
