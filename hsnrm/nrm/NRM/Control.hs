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

import Bandit.Class
import Bandit.Exp3
import CPD.Core
import CPD.Integrated
import CPD.Utils
import CPD.Values
import Control.Lens hiding ((...))
import Data.Generics.Product
import Data.Map as DM
import NRM.Orphans.NonEmpty ()
import NRM.Types.Controller
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
  Problem ->
  Input ->
  ControlM Decision
banditCartesianProductControl cpd (Reconfigure t) =
  case objective cpd of
    Nothing -> do
      field @"bandit" .= Nothing
      minTime <- use $ field @"integrator" . field @"minimumControlInterval"
      field @"integrator" .= initIntegrator t minTime (DM.keys $ sensors cpd)
      doNothing
    Just _ -> do
      minTime <- use $ field @"integrator" . field @"minimumControlInterval"
      field @"integrator" .= initIntegrator t minTime (DM.keys $ sensors cpd)
      maybeNonEmptyActionList & \case
        Nothing -> do
          field @"bandit" .= Nothing
          doNothing
        Just acp -> do
          g <- liftIO getStdGen
          let (b, a, g') = initPFMAB g (Arms acp)
          liftIO $ setStdGen g'
          field @"bandit" .= Just b
          return $ Decision a
  where
    maybeNonEmptyActionList =
      nonEmpty . cartesianProduct $
        DM.toList (actuators cpd)
          <&> \(actuatorID, a) -> Action actuatorID <$> actions a
banditCartesianProductControl cpd (NoEvent t) = tryControlStep cpd t
banditCartesianProductControl cpd (Event t ms) = do
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
  tryControlStep cpd t

tryControlStep ::
  Problem ->
  Time ->
  ControlM Decision
tryControlStep cpd t = case objective cpd of
  Nothing -> doNothing
  Just oexpr -> cStep oexpr (sensors cpd <&> range) t

cStep ::
  OExpr ->
  Map SensorID (Interval Double) ->
  Time ->
  ControlM Decision
cStep oexpr sensorRanges t =
  use (field @"integrator" . field @"measured") <&> squeeze t >>= \case
    Nothing -> doNothing
    Just (measurements, newMeasured) -> do
      field @"integrator" . field @"measured" .= newMeasured
      case (,) <$> eval measurements oexpr <*> evalRange sensorRanges oexpr of
        Nothing -> doNothing
        Just (value, range) ->
          case ZeroOneInterval <$> refine ((value - inf range) / width range) of
            Left _ -> doNothing
            Right v ->
              Decision
                <$> zoom
                  (field @"bandit" . _Just)
                  ( do
                      g <- liftIO getStdGen
                      (a, g') <- stepPFMAB g v
                      liftIO $ setStdGen g'
                      return a
                  )

doNothing :: ControlM Decision
doNothing = return DoNothing

-- | Cartesian product of lists. basically `sequence` with special treatment of
-- corner cases.
cartesianProduct :: [[Action]] -> [[Action]]
cartesianProduct [] = []
cartesianProduct [[]] = []
cartesianProduct s = sequence s
