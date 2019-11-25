{-# LANGUAGE DerivingVia #-}
{-# LANGUAGE TypeApplications #-}
{-# OPTIONS_GHC -fno-warn-orphans #-}

-- |
-- Module      : NRM.Control
-- Copyright   : (c) UChicago Argonne, 2019
-- License     : BSD3
-- Maintainer  : fre@freux.fr
module NRM.Control
  ( banditCartesianProductControl,
    ControlM,
  )
where

import CPD.Core
import CPD.Integrated
import CPD.Utils
import CPD.Values
import Control.Lens hiding ((...))
import Data.Generics.Product
import Data.Map as DM
import HBandit.BwCR as BwCR
import HBandit.Class
import HBandit.Types
import LMap.Map as LMap
import NRM.Orphans.NonEmpty ()
import NRM.Types.Configuration
import NRM.Types.Controller
import NRM.Types.MemBuffer as MemBuffer
import NRM.Types.NRM
import NRM.Types.Units
import Numeric.Interval
import Protolude
import Refined
import System.Random

-- | Zoomed NRM monad. Beware: can still `behave` and `ask`.
type ControlM a = App Controller a

-- |  basic control strategy - uses a bandit with a cartesian product
-- of admissible actuator actions as the the decision space.
banditCartesianProductControl ::
  ControlCfg ->
  Problem ->
  Input ->
  ControlM Decision
banditCartesianProductControl ccfg cpd (Reconfigure t) = do
  minTime <- use $ field @"integrator" . field @"minimumControlInterval"
  field @"integrator" .= initIntegrator t minTime (DM.keys $ sensors cpd)
  case objectives cpd of
    [] -> reset
    _ -> maybeNonEmptyActionList & \case
      Nothing -> reset
      Just acp -> do
        g <- liftIO getStdGen
        let (b, a, g') = learnCfg ccfg & \case
              LagrangeConstraints _ ->
                initPFMAB
                  g
                  (Arms acp)
                  & _1
                  %~ LagrangeConstraints
              KnapsackConstraints (BwCR.T gamma) ->
                initBwCR
                  g
                  (BwCRHyper gamma (Arms acp) undefined undefined)
                  & _1
                  %~ KnapsackConstraints
        liftIO $ setStdGen g'
        field @"bandit" .= Just b
        return $ Decision a
  where
    reset = do
      field @"bandit" .= Nothing
      field @"bufferedMeasurements" .= Nothing
      field @"referenceMeasurements" .= LMap.fromDataMap (sensors cpd $> MemBuffer.empty)
      refine 0 & \case
        Left _ -> logError "refinement failed in banditCartesianProductControl" >> doNothing
        Right v -> do
          field @"referenceMeasurementCounter" .= v
          doNothing
    maybeNonEmptyActionList =
      nonEmpty . cartesianProduct $
        DM.toList (actuators cpd)
          <&> \(actuatorID, a) -> Action actuatorID <$> actions a
banditCartesianProductControl ccfg cpd (NoEvent t) = tryControlStep ccfg cpd t
banditCartesianProductControl ccfg cpd (Event t ms) = do
  forM_ ms $ \(Measurement sensorID sensorValue sensorTime) -> do
    let s = DM.lookup sensorID (sensors cpd)
    s & \case
      Nothing -> return ()
      Just (range -> r) ->
        let v = (sensorTime, (sensorValue - inf r) / width r)
         in field @"integrator" %= \(Integrator tlast delta measuredM) ->
              Integrator
                tlast
                delta
                (measuredM & ix sensorID %~ measureValue (tlast + delta) v)
  tryControlStep ccfg cpd t

tryControlStep ::
  ControlCfg ->
  Problem ->
  Time ->
  ControlM Decision
tryControlStep ccfg cpd t = case objectives cpd of
  [] -> doNothing
  os -> wrappedCStep ccfg os (CPD.Core.constraints cpd) (DM.keys $ sensors cpd) t

type CStep =
  ControlCfg ->
  [(Double, OExpr)] ->
  [(Interval Double, OExpr)] ->
  [SensorID] ->
  Time ->
  ControlM Decision

wrappedCStep :: CStep
wrappedCStep ccfg stepObjectives stepConstraints sensors t = do
  counter <- use $ field @"referenceMeasurementCounter"
  let maxCounter = referenceMeasurementRoundInterval ccfg
  if unrefine counter < unrefine maxCounter
    then use (field @"bufferedMeasurements") >>= \case
      Nothing ->
        refine (unrefine counter + 1) & \case
          Left _ -> logError "refinement failed in wrappedCStep" >> doNothing
          Right value -> do
            field @"referenceMeasurementCounter" .= value
            cStep ccfg stepObjectives stepConstraints sensors t
      Just buffered -> undefined -- feed the buffered objective to the bandit and remove the buffered value.
    else undefined -- do the reference measurement

cStep :: CStep
cStep _ stepObjectives stepConstraints sensors t = do
  i <- use (field @"integrator")
  refMeasurements <- use (field @"referenceMeasurements")
  use (field @"integrator" . field @"measured") <&> squeeze t >>= \case
    Nothing -> doNothing
    Just (measurements, newMeasured) -> do
      field @"integrator" . field @"measured" .= newMeasured
      let evaluatedObjectives :: [(Double, Maybe Double, Maybe (Interval Double))]
          evaluatedObjectives = stepObjectives <&> \(w, v) ->
            (w, eval measurements v, evalRange (DM.fromList $ (,0 ... 1) <$> sensors) v)
          refinedObjectives :: Maybe [(Double, ZeroOne Double)]
          refinedObjectives =
            sequence
              ( evaluatedObjectives <&> \case
                  (w, Just v, Just r) -> normalize v (sup r) <&> (w,)
                  _ -> Nothing
              )
          evaluatedConstraints :: [(Interval Double, Maybe Double, Maybe Double)]
          evaluatedConstraints = stepConstraints <&> \(interv, c) ->
            ( interv,
              eval measurements c,
              eval (toDataMap refMeasurements <&> MemBuffer.avgBuffer) c
            )
          refinedConstraints :: Maybe [(Interval Double, Double)]
          refinedConstraints =
            sequence
              ( evaluatedConstraints <&> \case
                  (interv, Just v, Just ref) -> Just (interv, v / ref)
                  _ -> Nothing
              )
      (refinedObjectives, refinedConstraints) & \case
        (Just robjs, Just rconstr) -> do
          logInfo
            ( "aggregated measurement computed with: \n sensors:"
                <> show sensors
                <> "\n sensor values:"
                <> show measurements
                <> "\n refined constraints:"
                <> show robjs
                <> "\n refined objectives:"
                <> show rconstr
                <> "\n full integrator data structure:"
                <> show i
            )
          Decision
            <$> zoom
              (field @"bandit" . _Just)
              ( do
                  g <- liftIO getStdGen
                  (a, g') <-
                    get >>= \case
                      KnapsackConstraints b -> do
                        let ((a, g'), s') = runState (stepBwCR g ((snd <$> robjs) <> undefined)) b
                        put (KnapsackConstraints s')
                        return (a, g')
                      LagrangeConstraints b -> do
                        let ((a, g'), s') = runState (stepPFMAB g (hardConstrainedObjective robjs rconstr)) b
                        put (LagrangeConstraints s')
                        return (a, g')
                  liftIO $ setStdGen g'
                  return a
              )
        _ -> logError "objectives/constraints computation returned `Nothing`." >> doNothing

hardConstrainedObjective ::
  [(Double, ZeroOne Double)] ->
  [(Interval Double, Double)] ->
  ZeroOne Double
hardConstrainedObjective robjs rconstr =
  if allConstraintsMet
    then normalizedSum robjs
    else HBandit.Types.one
  where
    allConstraintsMet = all (\(i, v) -> Numeric.Interval.member v i) rconstr

doNothing :: ControlM Decision
doNothing = return DoNothing

-- | Cartesian product of lists. basically `sequence` with special treatment of
-- corner cases.
cartesianProduct :: [[Action]] -> [[Action]]
cartesianProduct [] = []
cartesianProduct [[]] = []
cartesianProduct s = sequence s
