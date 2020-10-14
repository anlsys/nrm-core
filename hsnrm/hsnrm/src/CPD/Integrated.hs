{-# LANGUAGE DerivingVia #-}
{-# LANGUAGE NoRecordWildCards #-}
{-# OPTIONS_GHC -fno-warn-partial-fields #-}

-- |
-- Module      : CPD.Integrated
-- Copyright   : (c) UChicago Argonne, 2019
-- License     : BSD3
-- Maintainer  : fre@freux.fr
module CPD.Integrated
  ( Integrator (..),
    IntegratorAction (..),
    MeasurementState (..),
    M (..),
    trapezoidArea,
    initIntegrator,
    throughTuple,
    measureValue,
    squeeze,
    averageArea,
  )
where

import CPD.Core
import Control.Lens
import qualified Data.Aeson as A
import Data.Data
import Data.Generics.Labels ()
import Data.JSON.Schema
import qualified Data.Map as M
import Data.MessagePack
import Dhall (Inject, Interpret)
import NRM.Classes.Messaging
import NRM.Types.Units
import Protolude

data Integrator
  = Integrator
      { tLast :: Time,
        minimumControlInterval :: Time,
        measured :: Map SensorID (MeasurementState M)
      }
  deriving (Generic)

data MeasurementState a
  = Never -- no value received in this measurement period yet
  | Discarded -- first value received and discarded, ready to start measuring
  | Running a -- measurements ongoing
  | Done a -- measurements complete, but still absorbing values
  deriving (Show, Eq, Data, MessagePack, Generic, Inject, Interpret, Functor)

instance Applicative MeasurementState where

  pure = Done

  Never <*> _ = Never
  Discarded <*> _ = Discarded
  Running f <*> m = fmap f m
  Done f <*> m = fmap f m

deriving via GenericJSON (MeasurementState a) instance (JSONSchema a) => JSONSchema (MeasurementState a)

deriving via GenericJSON (MeasurementState a) instance (A.ToJSON a) => A.ToJSON (MeasurementState a)

deriving via GenericJSON (MeasurementState a) instance (A.FromJSON a) => A.FromJSON (MeasurementState a)

data M
  = M
      { firstTime :: Time,
        lastTime :: Time,
        lastValue :: Double,
        area :: Double
      }
  deriving (Show, Eq, Data, MessagePack, Generic, Inject, Interpret)
  deriving
    (JSONSchema, A.ToJSON, A.FromJSON)
    via GenericJSON M

data IntegratorAction = IntegratorPasses | TriggerStep Integrator

trapezoidArea :: (Time, Double) -> (Time, Double) -> Double
trapezoidArea (t1, v1) (t2, v2) =
  if deltaT <= 0 then 0 else min v1 v2 * deltaT + (abs (v2 - v1) * deltaT / 2)
  where
    deltaT = fromuS (t2 - t1)

measureValue :: Time -> (Time, Double) -> MeasurementState M -> MeasurementState M
measureValue delta (newTime, newValue) = \case
  Never -> Discarded
  Discarded -> Running initial
  Done m -> Done $ measure m
  Running m ->
    ( if newTime >= delta + firstTime m
        then Done
        else Running
    )
      $ measure m
  where
    initial = M newTime newTime newValue 0
    measure = measureM newTime newValue

measureM :: Time -> Double -> M -> M
measureM newTime newValue M {firstTime, lastTime, lastValue, area} =
  M
    { firstTime = firstTime,
      lastTime = newTime,
      lastValue = newValue,
      area = trapezoidArea (lastTime, lastValue) (newTime, newValue) + area
    }

averageArea :: M -> Double
averageArea M {firstTime, lastTime, lastValue, area} =
  if deltaT <= 0 then lastValue else area / deltaT
  where
    deltaT = fromuS (lastTime - firstTime)

squeeze ::
  Time ->
  Map SensorID (MeasurementState M) ->
  Maybe (Map SensorID Double, Map SensorID (MeasurementState M))
squeeze _t mstM =
  case traverse throughTuple (M.toList mstM) of
    Done (M.fromList -> m) -> Just (m <&> averageArea, m $> Never)
    _ -> Nothing

throughTuple :: Functor f => (a, f b) -> f (a, b)
throughTuple (id, m) = (id,) <$> m

initIntegrator ::
  Time ->
  Time ->
  [SensorID] ->
  Integrator
initIntegrator t tmin sensorIDs =
  Integrator
    { tLast = t,
      minimumControlInterval = tmin,
      measured = M.fromList (sensorIDs <&> (,Never))
    }
