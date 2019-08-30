{-|
Module      : NRM.Actuators
Copyright   : (c) 2019, UChicago Argonne, LLC.
License     : BSD3
Maintainer  : fre@freux.fr
-}
module NRM.Actuators
  ( listActuators
  )
where

import CPD.Core
import NRM.Types.State
import Protolude

-- | List sensors
listActuators :: NRMState -> Map ActuatorID Actuator
listActuators _ = undefined
