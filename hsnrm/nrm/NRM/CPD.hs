{-|
Module      : NRM.CPD
Copyright   : (c) UChicago Argonne, 2019
License     : BSD3
Maintainer  : fre@freux.fr
-}
module NRM.CPD
  ( toCPD
  )
where

import CPD.Core
import NRM.Actuators
import NRM.Sensors
import NRM.Types.State
import Protolude
import qualified Data.Map as DM
import qualified NRM.Types.LMap as LM

toCPD :: NRMState -> Problem
toCPD = do
  sensors <- DM.fromList . LM.toList . cpdSensors
  actuators <- listActuators
  objective <- mkObjective
  return $ Problem {..}

mkObjective :: NRMState -> Objective
mkObjective _s = (Objective [] Minimize)
