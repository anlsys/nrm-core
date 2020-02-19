-- |
-- Module      : NRM.CPD
-- Copyright   : (c) UChicago Argonne, 2019
-- License     : BSD3
-- Maintainer  : fre@freux.fr
--
-- This module is responsible for runtime generation of the concrete CPD
-- description to be optimized by the control loop.
module NRM.CPD
  ( toCPD,
    throughputConstrained,
    addAll,
    maybeMinus,
  )
where

import CPD.Core
import Control.Lens hiding ((...))
import LMap.Map as DM
import LensMap.Core
import NRM.Actuators
import NRM.Sensors
import NRM.Types.Configuration
import NRM.Types.Sensor as S
import NRM.Types.State
import NRM.Types.Units
import Protolude hiding (Map)
import Refined (unrefine)

toCPD :: ControlCfg -> NRMState -> Problem
toCPD cfg st = Problem {..}
  where
    sensors = cpdSensors st
    actuators = cpdActuators st
    (objectives, constraints) = fromMaybe ([], []) (throughputConstrained <$> mcfg cfg <*> Just st)
    mcfg jc@ControlCfg {} = Just jc
    mcfg (FixedCommand _) = Nothing

throughputConstrained ::
  -- | Control configuration
  ControlCfg ->
  -- | State
  NRMState ->
  ( [(Double, OExpr)],
    [(Double, OExpr)]
  )
throughputConstrained cfg st =
  ( DM.toList toMinimize
      <&> \(i, _) -> (1, sID i \+ scalar (fromWatts $ staticPower cfg)),
    DM.toList constrained
      <&> \(i, _) -> (unrefine $ speedThreshold cfg, sID i)
  )
  where
    toMinimize :: Map SensorID SensorMeta
    toMinimize = DM.filterWithKey (\_ m -> Power `elem` tags m) allSensorMeta
    constrained :: Map SensorID SensorMeta
    constrained = DM.filterWithKey (\_ m -> DownstreamCmdSignal `elem` tags m) allSensorMeta
    allSensorMeta :: Map SensorID S.SensorMeta
    allSensorMeta =
      DM.fromList
        ( (bimap toS (\(ScopedLens l) -> meta (st ^. l)) <$> lA)
            <> (bimap toS (\(ScopedLens l) -> meta (st ^. l)) <$> lP)
        )
    lA = DM.toList (lenses st :: LensMap NRMState ActiveSensorKey ActiveSensor)
    lP = DM.toList (lenses st :: LensMap NRMState PassiveSensorKey PassiveSensor)

-- | produces a sum objective normalized by #sensors
addAll :: NonEmpty SensorID -> OExpr
addAll (s :| ss) =
  sumExpr \/ (scalar . fromIntegral $ length ss + 1)
  where
    sumExpr = foldl (\+) (sID s) (sID <$> ss)

-- | Subtract two objectives, defaulting to either of them
-- if one is absent.
maybeMinus :: Maybe OExpr -> Maybe OExpr -> Maybe OExpr
maybeMinus (Just x) (Just y) = Just $ x \- y
maybeMinus x Nothing = x
maybeMinus Nothing (Just x) = Just $ scalar 0 \- x
