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
import HBandit.BwCR as BwCR
import HBandit.Class
import HBandit.Types
import LMap.Map as DM
import NRM.Orphans.NonEmpty ()
import NRM.Types.Configuration
import NRM.Types.Controller
import NRM.Types.MemBuffer as MemBuffer
import NRM.Types.Messaging.UpstreamPub as UPub
import NRM.Types.NRM
import NRM.Types.Units
import Numeric.Interval
import Protolude hiding (Map, log)
import Refined
import Refined.Unsafe
import System.Random

-- | Zoomed NRM monad. Can still `behave` and `ask`.
type ControlM a = App Controller a

-- |  basic control strategy - uses a bandit with a cartesian product
-- of admissible actuator actions as the the decision space.
banditCartesianProductControl ::
  ControlCfg ->
  Problem ->
  Input ->
  Maybe [Action] ->
  ControlM Decision
banditCartesianProductControl ccfg cpd (Reconfigure t) _ = do
  pub (UPub.PubCPD t cpd)
  minTime <- use $ field @"integrator" . field @"minimumControlInterval"
  field @"integrator" .= initIntegrator t minTime (DM.keys $ sensors cpd)
  case CPD.Core.objectives cpd of
    [] -> reset
    _ -> maybeNonEmptyActionList & \case
      Nothing -> reset
      Just acp -> do
        logInfo "control: bandit initialization"
        g <- liftIO getStdGen
        let (b, a, g') = learnCfg ccfg & \case
              Lagrange _ ->
                initPFMAB
                  g
                  (Arms acp)
                  & _1
                  %~ Lagrange
              Knapsack (BwCR.T gamma) ->
                initBwCR
                  g
                  (BwCRHyper gamma (Arms acp) undefined undefined)
                  & _1
                  %~ Knapsack
        liftIO $ setStdGen g'
        field @"bandit" .= Just b
        return $ Decision a InitialDecision
  where
    reset = do
      logInfo "control: reset"
      field @"bandit" .= Nothing
      field @"bufferedMeasurements" .= Nothing
      field @"referenceMeasurements" .= (sensors cpd $> MemBuffer.empty)
      refine 0 & \case
        Left _ -> logError "refinement failed in banditCartesianProductControl" >> doNothing
        Right v -> do
          field @"referenceMeasurementCounter" .= v
          doNothing
    maybeNonEmptyActionList =
      nonEmpty . cartesianProduct $
        DM.toList (actuators cpd)
          <&> \(actuatorID, a) -> Action actuatorID <$> actions a
banditCartesianProductControl ccfg cpd (NoEvent t) mRefActions =
  tryControlStep ccfg cpd t mRefActions
banditCartesianProductControl ccfg cpd (Event t ms) mRefActions = do
  forM_ ms $ \m@(Measurement sensorID sensorValue sensorTime) -> do
    log $ "Processing measurement " <> show m
    field @"integrator" %= \(Integrator tlast delta measuredM) ->
      Integrator
        tlast
        delta
        ( measuredM
            & ix sensorID
              %~ measureValue (tlast + delta) (sensorTime, sensorValue)
        )
  tryControlStep ccfg cpd t mRefActions

tryControlStep ::
  ControlCfg ->
  Problem ->
  Time ->
  Maybe [Action] ->
  ControlM Decision
tryControlStep ccfg cpd t mRefActions = case CPD.Core.objectives cpd of
  [] -> doNothing
  os -> wrappedCStep ccfg os (CPD.Core.constraints cpd) (sensors cpd <&> range) t mRefActions

wrappedCStep ::
  ControlCfg ->
  [(Double, OExpr)] ->
  [(Double, OExpr)] ->
  Map SensorID (Interval Double) ->
  Time ->
  Maybe [Action] ->
  ControlM Decision
wrappedCStep cc stepObjectives stepConstraints sensorRanges t mRefActions = do
  logInfo "control: squeeze attempt"
  use (field @"integrator" . field @"measured") <&> squeeze t >>= \case
    Nothing -> logInfo "control: integrator squeeze failure" >> doNothing
    Just (measurements, newMeasured) -> do
      -- squeeze was successfull: setting the new integrator state
      logInfo "control: integrator squeeze success"
      field @"integrator" . field @"measured" .= newMeasured
      -- acquiring fields
      counter <- use $ field @"referenceMeasurementCounter"
      bufM <- use (field @"bufferedMeasurements")
      refM <- use $ field @"referenceMeasurements"
      let maxCounter = referenceMeasurementRoundInterval cc
      -- helper bindings
      let controlStep = stepFromSqueezed stepObjectives stepConstraints sensorRanges
      let innerStep = controlStep measurements
      refineLog (unrefine counter + 1) $ \counterValue ->
        case mRefActions of
          Nothing -> innerStep
          Just refActions -> do
            field @"referenceMeasurementCounter" .= counterValue
            -- define a ControlM monad value for starting the buffering
            let startBuffering = do
                  logInfo "control: meta control - starting reference measurement step"
                  -- reset the counter
                  field @"referenceMeasurementCounter" .= unsafeRefine 0
                  -- put the current measurements in "bufferedMeasurements"
                  field @"bufferedMeasurements" ?= measurements
                  -- take the reference actions
                  return $ Decision refActions ReferenceMeasurementDecision
            if unrefine counterValue <= unrefine maxCounter
              then case bufM of
                Nothing -> do
                  logInfo "control: inner control"
                  -- we perform the inner control step if no reference measurements
                  -- exist and otherwise run reference measurements.
                  DM.toList refM & \case
                    [] -> startBuffering
                    _ -> innerStep
                Just buffered -> do
                  -- we need to conclude this reference measurement mechanism
                  logInfo $
                    "control: meta control - concluding reference"
                      <> " measurement step, resuming inner control"
                  -- put the measurements that were just done in referenceMeasurements
                  field @"referenceMeasurements" %= enqueueAll measurements
                  -- clear the buffered measurements
                  field @"bufferedMeasurements" .= Nothing
                  -- feed the buffered measurements to the bandit
                  controlStep buffered
              else startBuffering
  where
    refineLog ::
      Predicate p x => x -> (Refined p x -> ControlM Decision) -> ControlM Decision
    refineLog v m = refine v & \case
      Left _ -> do
        logError "refinement failed in wrappedCStep"
        doNothing
      Right r -> m r

stepFromSqueezed ::
  [(Double, OExpr)] ->
  [(Double, OExpr)] ->
  Map SensorID (Interval Double) ->
  Map SensorID Double ->
  ControlM Decision
stepFromSqueezed stepObjectives stepConstraints sensorRanges measurements = do
  logInfo "in inner control"
  refMeasurements <- use (field @"referenceMeasurements")
  let evaluatedObjectives :: [(Double, Maybe Double, Maybe (Interval Double))]
      evaluatedObjectives = stepObjectives <&> \(w, v) ->
        (w, eval measurements v, evalRange sensorRanges v)
      refinedObjectives :: Maybe [(Double, ZeroOne Double)]
      refinedObjectives =
        sequence
          ( evaluatedObjectives <&> \case
              (w, Just v, Just r) -> normalize v (sup r) <&> (w,)
              _ -> Nothing
          )
      evaluatedConstraints :: [(Double, Maybe Double, Maybe Double)]
      evaluatedConstraints = stepConstraints <&> \(interv, c) ->
        ( interv,
          eval measurements c,
          eval (refMeasurements <&> MemBuffer.avgBuffer) c
        )
      refinedConstraints :: Maybe [(Double, Double)]
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
            <> show sensorRanges
            <> "\n sensor values:"
            <> show measurements
            <> "\n refined constraints:"
            <> show robjs
            <> "\n refined objectives:"
            <> show rconstr
        )
      use (field @"bandit") >>= \case
        Nothing -> doNothing
        Just bandit -> do
          g <- liftIO getStdGen
          (a, g', computedReward) <-
            bandit & \case
              Knapsack b -> do
                let ((a, g'), s') = runState (stepBwCR g ((snd <$> robjs) <> undefined)) b
                field @"bandit" ?= (Knapsack s')
                return (a, g', HBandit.Types.zero)
              Lagrange b -> do
                let hco = hardConstrainedObjective robjs rconstr
                log $ "computed Hard Constrained Objective of :" <> show hco
                let ((a, g'), s') = runState (stepPFMAB g hco) b
                field @"bandit" ?= (Lagrange s')
                return (a, g', hco)
          liftIO $ setStdGen g'
          return $
            Decision
              a
              ( InnerDecision
                  (ConstraintValue . snd <$> rconstr)
                  (ObjectiveValue . unrefine . snd <$> robjs)
                  computedReward
              )
    _ -> do
      logInfo $
        "controller failed a refinement step:"
          <> "\n stepObjectives: "
          <> show stepObjectives
          <> "\n stepConstraints: "
          <> show stepConstraints
          <> "\n sensorRanges: "
          <> show sensorRanges
          <> "\n measurements: "
          <> show measurements
          <> "\n refMeasurements: "
          <> show refMeasurements
          <> "\n refinedObjectives: "
          <> show refinedObjectives
          <> "\n refinedConstraints: "
          <> show refinedConstraints
          <> "\n evaluatedConstraints: "
          <> show evaluatedConstraints
          <> "\n evaluatedObjectives: "
          <> show evaluatedObjectives
      doNothing

hardConstrainedObjective ::
  [(Double, ZeroOne Double)] ->
  [(Double, Double)] ->
  ZeroOne Double
hardConstrainedObjective robjs rconstr =
  if allConstraintsMet
    then normalizedSum robjs
    else HBandit.Types.one
  where
    allConstraintsMet = all (\(threshold, v) -> v > threshold) rconstr

doNothing :: ControlM Decision
doNothing = return DoNothing

-- | Cartesian product of lists. Basically `sequence` with special treatment of
-- corner cases.
cartesianProduct :: [[Action]] -> [[Action]]
cartesianProduct [] = []
cartesianProduct [[]] = []
cartesianProduct s = sequence s
