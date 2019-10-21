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
import LensMap.Core
import NRM.Actuators
import NRM.Sensors
import NRM.Types.Sensor
import NRM.Types.State
import Protolude

toCPD :: NRMState -> Problem
toCPD = do
  sensors <- cpdSensors
  actuators <- cpdActuators
  objective <- mkObjective
  return $ Problem {..}

mkObjective :: NRMState -> Objective
mkObjective st =
  lAf <> lPf & \case
    [] -> Nothing
    v : vs -> Just (foldl (OAdd) v vs)
  where
    lA = DM.keys (lenses st :: LensMap NRMState ActiveSensorKey ActiveSensor)
    lP = DM.keys (lenses st :: LensMap NRMState PassiveSensorKey PassiveSensor)
    lAf = filter filterActive lA <&> OValue . toS
    lPf = filter filterPassive lP <&> OValue . toS
    filterActive :: ActiveSensorKey -> Bool
    filterActive _ = True
    filterPassive :: PassiveSensorKey -> Bool
    filterPassive _ = True

--filterWithKey :: (k -> a -> Bool) -> Map k a -> Map k a
