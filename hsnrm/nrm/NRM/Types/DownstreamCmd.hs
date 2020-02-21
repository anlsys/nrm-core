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
import Data.Aeson hiding ((.=))
import Data.Generics.Product
import Data.JSON.Schema
import Data.MessagePack
import LMap.Map as DM
import LensMap.Core
import NRM.Classes.Messaging
import NRM.Types.DownstreamCmdID
import NRM.Types.MemBuffer
import NRM.Types.Sensor
import NRM.Types.Units as Units
import Numeric.Interval
import Protolude hiding (to)

data DownstreamCmd
  = DownstreamCmd
      { maxValue :: Units.Operations,
        ratelimit :: Units.Frequency,
        dtLastReferenceMeasurements :: MemBuffer Double,
        lastRead :: Maybe (Time, Progress)
      }
  deriving (Show, Generic, MessagePack)
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
      getter (DownstreamCmd _maxValue ratelimit dtLastRef lastRead) =
        ActiveSensor
          { activeMeta = SensorMeta
              { tags = [Maximize, DownstreamCmdSignal],
                range = 0 ... (maxValue downstreamCmd & fromOps & fromIntegral),
                lastReferenceMeasurements = dtLastRef,
                last = lastRead <&> fmap (fromIntegral . fromProgress),
                cumulative = IntervalBased
              },
            maxFrequency = ratelimit,
            process = identity
          }
      setter dc activeSensor =
        dc &~ do
          field @"maxValue"
            .= activeSensor ^. _meta . field @"range" . to sup . to floor . to Operations
          field @"dtLastReferenceMeasurements" .= activeSensor ^. _meta . to lastReferenceMeasurements
          field @"lastRead" .= over _Just (over _2 $ progress . floor) (activeSensor ^. _meta . field @"last")
