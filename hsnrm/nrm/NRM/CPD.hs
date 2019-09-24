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
import qualified Data.Map as DM
import NRM.Actuators
import NRM.Sensors
import qualified NRM.Types.LMap as LM
import NRM.Types.State
import Protolude

toCPD :: NRMState -> Problem
toCPD = do
  sensors <- DM.fromList . LM.toList . cpdSensors
  actuators <- DM.fromList . LM.toList . cpdActuators
  objective <- mkObjective
  return $ Problem {..}

mkObjective :: NRMState -> Objective
mkObjective _s = (Objective [] Minimize)
