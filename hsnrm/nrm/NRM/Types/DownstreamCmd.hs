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

import Control.Lens
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
      {  maxValue :: Units.Operations
      , ratelimit :: Units.Frequency
      }
  deriving (Show, Generic, Data, MessagePack)
  deriving
    (JSONSchema, ToJSON, FromJSON)
    via GenericJSON DownstreamCmd

instance
  HasLensMap (DownstreamCmdID, DownstreamCmd)
    ActiveSensorKey
    ActiveSensor where

  lenses (downstreamCmdID, downstreamCmd) =
    DM.singleton
      (DownstreamCmdKey downstreamCmdID)
      (ScopedLens (_2 . lens getter setter))
    where
      getter (DownstreamCmd maxValue ratelimit) =
        Just $ ActiveSensor
          { activeTags = [Tag "perf"]
          , activeSource = Source $ show downstreamCmdID
          , activeRange = (0, 1)
          , maxFrequency = ratelimit
          , process = identity
          }
      setter dc (Just activeSensor) =
        dc & field @"maxValue" .~
          Operations (floor $ snd $ activeRange activeSensor)
      setter dc Nothing = dc
