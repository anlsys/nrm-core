{-# LANGUAGE DerivingVia #-}
{-# LANGUAGE TypeApplications #-}
{-# OPTIONS_GHC -fno-warn-missing-local-signatures #-}

-- |
-- Module      : NRM.Types.DownstreamCmd
-- Copyright   : (c) UChicago Argonne, 2019
-- License     : BSD3
-- Maintainer  : fre@freux.fr
module NRM.Types.DownstreamCmd
  ( DownstreamCmd (..),
  )
where

import Control.Lens hiding ((...))
import Data.Aeson
import Data.Data
import Data.Generics.Product
import Data.JSON.Schema
import Data.Map as DM
import Data.MessagePack
import LensMap.Core
import NRM.Classes.Messaging
import NRM.Types.DownstreamCmdID
import NRM.Types.Sensor as S
import NRM.Types.Units as Units
import Numeric.Interval
import Protolude

data DownstreamCmd
  = DownstreamCmd
      { maxValue :: Units.Operations,
        ratelimit :: Units.Frequency
      }
  deriving (Show, Generic, Data, MessagePack)
  deriving
    (JSONSchema, ToJSON, FromJSON)
    via GenericJSON DownstreamCmd

instance
  HasLensMap (DownstreamCmdID, DownstreamCmd)
    ActiveSensorKey
    ActiveSensor
  where
  lenses (downstreamCmdID, downstreamCmd) =
    DM.singleton
      (DownstreamCmdKey downstreamCmdID)
      (ScopedLens (_2 . lens getter setter))
    where
      getter (DownstreamCmd _maxValue ratelimit) =
        ActiveSensor
          { activeTags = S.Tag S.Maximize [S.DownstreamCmdSignal],
            activeRange = 0 ... (maxValue downstreamCmd & fromOps & fromIntegral),
            maxFrequency = ratelimit,
            process = identity
          }
      setter dc activeSensor =
        dc & field @"maxValue"
          .~ Operations (floor . sup $ activeRange activeSensor)
