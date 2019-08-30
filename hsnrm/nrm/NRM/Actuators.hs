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

import NRM.Types.Actuator
import NRM.Types.State
{-import Protolude-}

-- | List sensors
listActuators :: NRMState -> [Actuator]
listActuators _ = []
