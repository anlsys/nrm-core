{-|
Module      : NRM.Actuators
Copyright   : (c) 2019, UChicago Argonne, LLC.
License     : BSD3
Maintainer  : fre@freux.fr
-}
module NRM.Actuators
  ( cpdActuators
  )
where

import CPD.Core as CPD
import NRM.Classes.Actuators as CA
import LMap.Map as LM
import NRM.Types.State
import Protolude

cpdActuators :: NRMState -> LM.Map CPD.ActuatorID CPD.Actuator
cpdActuators s = undefined
