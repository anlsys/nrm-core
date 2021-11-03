{-# LANGUAGE DerivingVia #-}

-- |
-- Module      : CPD.Values
-- Copyright   : (c) UChicago Argonne, 2019
-- License     : BSD3
-- Maintainer  : swann@anl.gov
module CPD.Values
  ( Measurement (..),
    Action (..),
  )
where

import CPD.Core
import qualified Data.Aeson as A
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
  deriving (Show, Generic, MessagePack, Interpret, Inject)
  deriving (JSONSchema, A.ToJSON, A.FromJSON) via GenericJSON Measurement

data Action
  = Action
      { actuatorID :: ActuatorID,
        actuatorValue :: Discrete
      }
  deriving (Ord, Show, Eq, Generic, MessagePack, Interpret, Inject)
  deriving (JSONSchema, A.ToJSON, A.FromJSON) via GenericJSON Action
