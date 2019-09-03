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
import Data.Map as DM
import qualified NRM.Types.Sensor as S
import NRM.Types.State
import Protolude

-- | List sensors
listSensors :: NRMState -> Map SensorID Sensor
listSensors = listPackageSensors

-- | List sensors
listPackageSensors :: NRMState -> Map SensorID Sensor
listPackageSensors s =
  DM.fromList $ uncurry S.toCPDPackedSensor <$>
    DM.toList
      ( mconcat $
        uncurry S.listSensors <$>
        DM.toList (packages s)
      )
