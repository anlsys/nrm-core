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
import Data.Map as DM
import NRM.Actuators
import NRM.Sensors
import NRM.Types.State
import Protolude

toCPD :: NRMState -> Problem
toCPD s =
  Problem
    (DM.toList (listNRMSensors s) <&> uncurry SensorKV)
    (DM.toList (listActuators s) <&> uncurry ActuatorKV)
    (Objective [] Minimize)
