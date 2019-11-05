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
  )
where

import Bandit.Class
import Bandit.Exp3
import CPD.Core
import CPD.Integrated
import CPD.Utils
import CPD.Values
import Control.Lens
import Data.Generics.Product
import Data.Map as DM
import NRM.Orphans.NonEmpty ()
import NRM.Types.Controller
import NRM.Types.Units
import Numeric.Interval
import Protolude
import Refined
import System.Random

-- |  basic control strategy - uses a bandit with a cartesian product
-- of admissible actuator actions as the the decision space.
banditCartesianProductControl ::
  ( Applicative (Zoomed m0 [Action]),
    Zoom m0 m (Exp3 [Action]) Controller,
    MonadIO m,
    MonadIO m0
  ) =>
  Input ->
  m Decision
banditCartesianProductControl (Reconfigure t cpd) =
  integrateProblem cpd & \case
    Nothing -> do
      field @"integratedProblem" .= Nothing
      field @"bandit" .= Nothing
      minTime <- use $ field @"integrator" . field @"minimumControlInterval"
      field @"integrator" .= initIntegrator t minTime
      doNothing
    Just ipb -> do
      field @"integratedProblem" .= Just ipb
      minTime <- use $ field @"integrator" . field @"minimumControlInterval"
      field @"integrator" .= initIntegrator t minTime
      nonEmpty
        ( cartesianProduct
            ( DM.toList (iactuators ipb)
                <&> \(actuatorID, a) -> Action actuatorID <$> actions a
            )
        )
        & \case
          Nothing -> do
            field @"bandit" .= Nothing
            doNothing
          Just acp -> do
            g <- liftIO getStdGen
            let (b, a, g') = initPFMAB g (Arms acp)
            liftIO $ setStdGen g'
            field @"bandit" .= Just b
            return $ Decision a
banditCartesianProductControl (NoEvent t) = tryControlStep t
banditCartesianProductControl (Event t ms) = do
  for_ ms $ \(Measurement sensorID sensorValue sensorTime) ->
    field @"integrator" %= \(Integrator tlast delta measuredM) ->
      Integrator
        tlast
        delta
        (measuredM & ix sensorID %~ measureValue (tlast + delta) (sensorTime, sensorValue))
  tryControlStep t

tryControlStep ::
  ( Applicative (Zoomed m0 [Action]),
    Zoom m0 m (Exp3 [Action]) Controller,
    MonadIO m0
  ) =>
  Time ->
  m Decision
tryControlStep t =
  use (field @"integratedProblem") >>= \case
    Nothing -> doNothing
    Just ipb -> use (field @"integrator" . field @"measured")
      <&> squeeze t
      >>= \case
        Nothing -> doNothing
        Just (measurements, newMeasured) -> do
          field @"integrator" . field @"measured" .= newMeasured
          let iobj = iobjective ipb
          case (,) <$> (eval measurements =<< iobj) <*> (evalRange (isensors ipb) =<< iobj) of
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

doNothing :: (Monad m) => m Decision
doNothing = return DoNothing

-- | Cartesian product of lists. basically `sequence` with special treatment of
-- corner cases.
cartesianProduct :: [[Action]] -> [[Action]]
cartesianProduct [] = []
cartesianProduct [[]] = []
cartesianProduct s = sequence s
