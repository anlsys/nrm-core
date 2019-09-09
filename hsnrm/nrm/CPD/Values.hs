{-# LANGUAGE DerivingVia #-}

{-|
Module      : CPD.Values
Copyright   : (c) UChicago Argonne, 2019
License     : BSD3
Maintainer  : fre@freux.fr
-}
module CPD.Values
  ( Value (..)
  , SensorData (..)
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
import Protolude

data Value
  = DiscreteValue {discreteValue :: Discrete}
  | ContinuousValue {continuousValue :: Double}
  deriving (JSONSchema, A.ToJSON, A.FromJSON) via GenericJSON Value
  deriving (Show, Generic, Data, MessagePack, Interpret, Inject)

data SensorData
  = SensorData
      { sensorID :: SensorID
      , sensorValue :: Value
      }
  deriving (JSONSchema, A.ToJSON, A.FromJSON) via GenericJSON SensorData
  deriving (Show, Generic, Data, MessagePack, Interpret, Inject)

