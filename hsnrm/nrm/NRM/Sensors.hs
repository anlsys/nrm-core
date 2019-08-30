{-|
Module      : NRM.Sensors
Copyright   : (c) 2019, UChicago Argonne, LLC.
License     : BSD3
Maintainer  : fre@freux.fr
-}
module NRM.Sensors
  ( listSensors
  )
where

import NRM.Types.Sensor
import NRM.Types.State
import Protolude

-- | List sensors
listSensors :: NRMState -> [Sensor]
listSensors s = listPackageSensors s <> []

-- | List sensors
listPackageSensors :: NRMState -> [Sensor]
listPackageSensors _ = []
