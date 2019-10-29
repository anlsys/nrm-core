{-# LANGUAGE DerivingVia #-}
{-# LANGUAGE TypeApplications #-}

-- |
-- Module      : NRM.Control
-- Copyright   : (c) UChicago Argonne, 2019
-- License     : BSD3
-- Maintainer  : fre@freux.fr
module NRM.Control
  ( control,
  )
where

import CPD.Integrated
import Control.Lens
import Data.Generics.Product
import NRM.Orphans.NonEmpty ()
import NRM.Types.Controller
import Protolude

control :: (MonadState Controller m) => Input -> m Decision
control (Reconfigure t cpd) = do
  let ipb = integrateProblem cpd
  field @"integratedProblem" .= Just ipb
  field @"integrator" .= initIntegrator t ipb
  return DoNothing
-- data Controller
--   = Controller
--       { cpd :: Maybe C.IntegratedProblem,
--         integrator :: C.Integrator,
--         bandit :: Maybe (Exp3 SensorID)
--       }
--
