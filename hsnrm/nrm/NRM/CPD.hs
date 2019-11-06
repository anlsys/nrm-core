-- |
-- Module      : NRM.CPD
-- Copyright   : (c) UChicago Argonne, 2019
-- License     : BSD3
-- Maintainer  : fre@freux.fr
module NRM.CPD
  ( toCPD,
  )
where

import CPD.Core
import Control.Lens
import qualified Data.Map as DM
import LensMap.Core
import NRM.Actuators
import NRM.Sensors
import NRM.Types.Sensor as S
import NRM.Types.State
import Protolude

toCPD :: NRMState -> Problem
toCPD = do
  sensors <- cpdSensors
  actuators <- cpdActuators
  objective <- defaultObjective
  return $ Problem {..}

defaultObjective :: NRMState -> Objective
defaultObjective st = addAll toMinimize `maybeMinus` addAll toMaximize
  where
    toMinimize :: [SensorID]
    toMinimize = DM.keys (DM.filterWithKey (\_ v -> v == Minimize) directionMap)
    toMaximize :: [SensorID]
    toMaximize = DM.keys (DM.filterWithKey (\_ v -> v == Maximize) directionMap)
    directionMap :: Map SensorID S.Direction
    directionMap =
      DM.fromList
        ( (bimap toS (\(ScopedLens l) -> (S.standardDirection . activeTags) (st ^. l)) <$> lA)
            <> (bimap toS (\(ScopedLens l) -> (S.standardDirection . passiveTags) (st ^. l)) <$> lP)
        )
    lA = DM.toList (lenses st :: LensMap NRMState ActiveSensorKey ActiveSensor)
    lP =
      DM.toList (lenses st :: LensMap NRMState PassiveSensorKey PassiveSensor)

-- | produces a sum objective normalized by #sensors
addAll :: [SensorID] -> Objective
addAll [] = Nothing
addAll (s : ss) =
  Just $
    sumExpr \/ (scalar . fromIntegral $ length ss + 1)
  where
    sumExpr = foldl (\+) (sID s) (sID <$> ss)

-- | Subtract two objectives, defaulting to either of them
-- if one is absent.
maybeMinus :: Objective -> Objective -> Objective
maybeMinus (Just x) (Just y) = Just $ x \- y
maybeMinus x Nothing = x
maybeMinus Nothing (Just x) = Just $ scalar 0 \- x
