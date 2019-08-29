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

import NRM.Types.Sensor
import NRM.Types.State

-- | List sensors
listActuators :: NRMState -> [Sensor]
listActuators = [Sensor.Sensor]
