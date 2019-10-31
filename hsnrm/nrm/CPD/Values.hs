{-# LANGUAGE DerivingVia #-}

-- |
-- Module      : CPD.Values
-- Copyright   : (c) UChicago Argonne, 2019
-- License     : BSD3
-- Maintainer  : fre@freux.fr
module CPD.Values
  ( Measurement (..),
    Action (..),
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
      { sensorID :: SensorID,
        sensorValue :: Double,
        time :: Time
      }
  deriving (Show, Generic, Data, MessagePack, Interpret, Inject)
  deriving (JSONSchema, A.ToJSON, A.FromJSON) via GenericJSON Measurement

data Action
  = Action
      { actuatorID :: ActuatorID,
        actuatorValue :: Discrete
      }
  deriving (Show, Eq, Generic, Data, MessagePack, Interpret, Inject)
  deriving (JSONSchema, A.ToJSON, A.FromJSON) via GenericJSON Action
