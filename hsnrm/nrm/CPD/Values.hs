{-# LANGUAGE DerivingVia #-}

{-|
Module      : CPD.Values
Copyright   : (c) UChicago Argonne, 2019
License     : BSD3
Maintainer  : fre@freux.fr
-}
module CPD.Values
  ( Value (..)
  , Measurement (..)
  , Action (..)
  , Measurements (..)
  , Actions (..)
  )
where

import CPD.Core
import qualified Data.Aeson as A
import Data.Data
import Data.JSON.Schema
import Data.MessagePack
import Dhall
import NRM.Classes.Messaging
import NRM.Types.Units
import NRM.Orphans.UUID ()
import Protolude

data Value
  = DiscreteValue {discreteValue :: Discrete}
  | ContinuousValue {continuousValue :: Double}
  deriving (JSONSchema, A.ToJSON, A.FromJSON) via GenericJSON Value
  deriving (Show, Generic, Data, MessagePack, Interpret, Inject)

data Measurement
  = Measurement
      { sensorID :: SensorID
      , sensorValue :: Value
      , time :: Time
      }
  deriving (JSONSchema, A.ToJSON, A.FromJSON) via GenericJSON Measurement
  deriving (Show, Generic, Data, MessagePack, Interpret, Inject)

data Action
  = Action
      { actuatorID :: ActuatorID
      , actuatorValue :: Value
      }
  deriving (JSONSchema, A.ToJSON, A.FromJSON) via GenericJSON Action
  deriving (Show, Generic, Data, MessagePack, Interpret, Inject)

newtype Measurements = Measurements [Measurement]
  deriving (JSONSchema, A.ToJSON, A.FromJSON, Interpret, Inject) via [Measurement]
  deriving (Show, Generic, Data, MessagePack)

newtype Actions = Actions [Action]
  deriving (JSONSchema, A.ToJSON, A.FromJSON, Interpret, Inject) via [Action]
  deriving (Show, Generic, Data, MessagePack)
