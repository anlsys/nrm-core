{-# LANGUAGE DerivingVia #-}

{-|
Module      : CPD.Values
Copyright   : (c) UChicago Argonne, 2019
License     : BSD3
Maintainer  : fre@freux.fr
-}
module CPD.Values
  ( Measurement (..)
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
import NRM.Orphans.UUID ()
import NRM.Types.Units
import Protolude

data Measurement
  = Measurement
      { sensorID :: SensorID
      , sensorValue :: Double
      , time :: Time
      }
  deriving (JSONSchema, A.ToJSON, A.FromJSON) via GenericJSON Measurement
  deriving (Show, Generic, Data, MessagePack, Interpret, Inject)

data Action
  = Action
      { actuatorID :: ActuatorID
      , actuatorValue :: Discrete
      }
  deriving (JSONSchema, A.ToJSON, A.FromJSON) via GenericJSON Action
  deriving (Show, Generic, Data, MessagePack, Interpret, Inject)

newtype Measurements = Measurements [Measurement]
  deriving (JSONSchema, A.ToJSON, A.FromJSON, Interpret, Inject) via [Measurement]
  deriving (Show, Generic, Data, MessagePack)

newtype Actions = Actions [Action]
  deriving (JSONSchema, A.ToJSON, A.FromJSON, Interpret, Inject) via [Action]
  deriving (Show, Generic, Data, MessagePack)
