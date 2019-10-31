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
import Control.Monad.Trans.RWS.Lazy (RWST)
import Data.Generics.Product
import Data.Map as DM
import Data.Random
import NRM.Orphans.NonEmpty ()
import NRM.Types.Behavior (Behavior)
import NRM.Types.Configuration (Cfg)
import NRM.Types.Controller
import NRM.Types.Units
import Numeric.Interval
import Protolude
import Refined

-- |  basic control strategy - uses a bandit with a cartesian product
-- of admissible actuator actions as the the decision space.
banditCartesianProductControl ::
  ( Applicative (Zoomed m0 ([Action])),
    Zoom m0 m (Exp3 [Action]) Controller,
    MonadRandom m,
    MonadRandom m0
  ) =>
  Input ->
  m Decision
banditCartesianProductControl (Reconfigure t cpd) =
  integrateProblem cpd & \case
    Nothing -> do
      field @"integratedProblem" .= Nothing
      field @"bandit" .= Nothing
      freq <- use $ (field @"integrator") . (field @"maximumControlFrequency")
      field @"integrator" .= initIntegrator t freq
      doNothing
    Just ipb -> do
      field @"integratedProblem" .= Just ipb
      freq <- use $ (field @"integrator") . (field @"maximumControlFrequency")
      field @"integrator" .= initIntegrator t freq
      nonEmpty
        ( sequence $
            DM.toList (iactuators ipb)
              <&> \(actuatorID, actions -> discretes) ->
                [(Action actuatorID d) | d <- discretes]
        )
        & \case
          Nothing -> do
            field @"bandit" .= Nothing
            doNothing
          Just acp -> do
            (b, a) <- initPFMAB (Arms acp)
            field @"bandit" .= Just b
            return $ Decision a
banditCartesianProductControl (NoEvent t) = tryControlStep t
banditCartesianProductControl (Event t ms) = do
  for_ ms $ \(Measurement sensorID sensorValue sensorTime) ->
    field @"integrator"
      . field @"measured"
      . (at sensorID)
      . _Just <>= [(sensorTime, sensorValue)]
  tryControlStep t

tryControlStep ::
  ( Applicative (Zoomed m0 ([Action])),
    Zoom m0 m (Exp3 [Action]) Controller,
    MonadRandom m0
  ) =>
  Time ->
  m Decision
tryControlStep t = do
  i <- use (field @"integrator")
  use (field @"integratedProblem") >>= \case
    Nothing -> doNothing
    Just ipb -> (calculate t i ipb) & \case
      Nothing -> doNothing
      Just (Calculate remains measurements) -> do
        field @"integrator" . field @"measured" .= remains
        let iobj = iobjective ipb
        case (,) <$> (eval measurements =<< iobj) <*> (evalRange (isensors ipb) =<< iobj) of
          Nothing -> doNothing
          Just (value, range) ->
            case ZeroOneInterval <$> refine ((value - inf range) / (width range)) of
              Left _ -> doNothing
              Right v ->
                zoom (field @"bandit" . _Just) (stepPFMAB v)
                  >>= return . Decision

doNothing :: (Monad m) => m Decision
doNothing = return DoNothing

instance MonadRandom (RWST Cfg [Behavior] s IO)
