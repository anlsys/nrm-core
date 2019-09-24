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
import NRM.Types.LMap as LM
import NRM.Types.State
import Protolude

cpdActuators :: NRMState -> LMap CPD.ActuatorID CPD.Actuator
cpdActuators s = (CA.actuators s) & LM.mapKV toCPDActuator
