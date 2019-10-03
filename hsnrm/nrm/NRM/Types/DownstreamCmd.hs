{-# LANGUAGE DerivingVia #-}

{-|
Module      : NRM.Types.DownstreamCmd
Copyright   : (c) UChicago Argonne, 2019
License     : BSD3
Maintainer  : fre@freux.fr
-}
module NRM.Types.DownstreamCmd
  ( DownstreamCmd (..)
  )
where

import Data.Aeson
import Data.Data
import Data.Generics.Product
import Data.JSON.Schema
import Data.Map as DM
import Data.MessagePack
import Data.String (IsString (..))
import qualified Data.UUID as U
import LensMap.Core
import NRM.Classes.Messaging
import NRM.Classes.Sensors
import NRM.Types.DownstreamCmdID
import NRM.Types.Sensor
import NRM.Types.Units as Units
import Protolude
import Prelude (fail)

data DownstreamCmd
  = DownstreamCmd
      { id :: SensorID
      , maxValue :: Units.Operations
      }
  deriving (Eq, Ord, Show, Generic, Data, MessagePack)
  deriving
    (JSONSchema, ToJSON, FromJSON)
    via GenericJSON DownstreamCmd

instance HasLensMap (DownstreamCmdID, DownstreamCmd) ActiveSensorKey ActiveSensor where

  {-LM.map-}
  {-( \dc ->-}
  {-if DC.id dc == sensorID-}
  {-then dc & field @"maxValue" .~ (Operations $ floor b)-}
  {-else dc-}
  {-)-}
  lenses (downstreamCmdID, downstreamCmd) =
    DM.singleton
      ( DownstreamCmdKey downstreamCmdID
      , ScopedLens (_2 . field @"rapl" . lens getter setter)
      )
    where
      getter (DownstreamCmd id maxValue) =
        Just $ ActiveSensor
          { activeTags = [Tag "perf"]
          , activeSource = Source $ show cmdID
          , activeRange = (0, 1)
          , maxFrequency = ratelimit $ monitoring $ app $ manifest cmdCore
          , process = identity
          }
        where
          textID = show packageID
      setter rapl (Just passiveSensor) =
        Just $ rapl & field @"max" .~ MaxEnergy (uJ (snd $ passiveRange passiveSensor))
      setter _rapl Nothing = Nothing
