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

import CPD.Core
{-import NRM.Types.Sensor-}
import Data.Map as DM
import NRM.Types.State

{-import Protolude-}

-- | List sensors
listSensors :: NRMState -> Map SensorID Sensor
listSensors = listPackageSensors

-- | List sensors
listPackageSensors :: NRMState -> Map SensorID Sensor
listPackageSensors _ = DM.fromList []
