-- |
-- Module      : NRM.CPD
-- Copyright   : (c) UChicago Argonne, 2019
-- License     : BSD3
-- Maintainer  : fre@freux.fr
--
-- This module is responsible for runtime generation of the CPD
-- description to be optimized by the control loop.
module NRM.CPD
  ( toCPD,
    throughputConstrained,
    addAll,
    maybeMinus,
  )
where

import Bandit.Types
import CPD.Core
import Control.Lens hiding ((...))
import Data.Coerce
import LMap.Map as LM
import LensMap.Core
import NRM.Actuators
import NRM.Sensors
import NRM.Types.Configuration
import NRM.Types.Sensor as S
import NRM.Types.State
import NRM.Types.Units
import Protolude hiding (Map)

toCPD :: ControlCfg -> NRMState -> Problem
toCPD cfg st = Problem {..}
  where
    sensors = cpdSensors st
    actuators = cpdActuators st
    (objectives, constraints) = fromMaybe ([], []) (throughputConstrained <$> mcfg cfg <*> Just st)
    mcfg jc@ControlCfg {} = Just jc
    mcfg _ = Nothing

-- | This problem generator produces a global energy minimization problem under a
-- throughput constraint.
throughputConstrained ::
  -- | Control configuration
  ControlCfg ->
  -- | State
  NRMState ->
  ( [(ZeroOne Double, OExpr)],
    [(Double, OExpr)]
  )
throughputConstrained cfg st =
  ( idsToMinimize & \case
      Nothing -> []
      Just ids ->
        let powerTerm =
              coerce (foldMap (OExprSum . sID) ids)
                \+ scalar (fromWatts $ staticPower cfg)
         in [(Bandit.Types.one, maybe powerTerm (powerTerm \/) normalizedSumSlowdown)],
    normalizedSumSlowdown & \case
      Nothing -> []
      Just expr -> [(speedThreshold cfg, expr)]
  )
  where
    normalizedSumSlowdown :: Maybe OExpr
    normalizedSumSlowdown =
      nonEmpty (LM.toList constrained) <&> \(fmap fst -> ids) ->
        thresholded 0.5 1.5 (coerce (foldMap (OExprSum . sID) ids) \/ (coerce (foldMap (OExprSum . sRef) ids) \+ scalar 1))
    idsToMinimize :: Maybe (NonEmpty SensorID)
    idsToMinimize = nonEmpty (fst <$> LM.toList toMinimize)
    toMinimize :: Map SensorID SensorMeta
    toMinimize = LM.filterWithKey (\_ m -> Power `elem` S.tags m) allSensorMeta
    constrained :: Map SensorID SensorMeta
    constrained = LM.filterWithKey (\_ m -> DownstreamCmdSignal `elem` S.tags m) allSensorMeta
    allSensorMeta :: Map SensorID S.SensorMeta
    allSensorMeta =
      LM.fromList
        ( (bimap toS (\(ScopedLens l) -> st ^. l . _meta) <$> lA)
            <> (bimap toS (\(ScopedLens l) -> st ^. l . _meta) <$> lP)
        )
    lA = LM.toList (lenses st :: LensMap NRMState ActiveSensorKey ActiveSensor)
    lP = LM.toList (lenses st :: LensMap NRMState PassiveSensorKey PassiveSensor)

-- | produces a sum objective normalized by #sensors
addAll :: NonEmpty SensorID -> OExpr
addAll ss = coerce (foldMap (OExprSum . sID) ss) \/ (scalar . fromIntegral $ length ss)

-- | Subtract two objectives, defaulting to either of them
-- if one is absent.
maybeMinus :: Maybe OExpr -> Maybe OExpr -> Maybe OExpr
maybeMinus (Just x) (Just y) = Just $ x \- y
maybeMinus x Nothing = x
maybeMinus Nothing (Just x) = Just $ scalar 0 \- x
