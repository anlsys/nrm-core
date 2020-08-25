{-# LANGUAGE DerivingVia #-}

-- |
-- Module      : NRM.Types.DownstreamThread
-- Copyright   : (c) UChicago Argonne, 2019
-- License     : BSD3
-- Maintainer  : fre@freux.fr
module NRM.Types.DownstreamThread
  ( DownstreamThread (..),
  )
where

import Control.Lens hiding ((...))
import Data.Aeson hiding ((.=))
import Data.Generics.Labels ()
import Data.JSON.Schema
import Data.MessagePack
import Data.Map as M
import LensMap.Core
import NRM.Classes.Messaging
import NRM.Types.DownstreamThreadID
import NRM.Types.MemBuffer
import NRM.Types.Sensor
import NRM.Types.Units
import Numeric.Interval
import Protolude hiding (to)

data DownstreamThread
  = DownstreamThread
      { maxValue :: Progress,
        ratelimit :: Frequency,
        dtLastReferenceMeasurements :: MemBuffer,
        lastRead :: Maybe (Time, Progress)
      }
  deriving (Eq, Ord, Show, Generic, MessagePack)
  deriving (JSONSchema, ToJSON, FromJSON) via GenericJSON DownstreamThread

instance
  HasLensMap (DownstreamThreadID, DownstreamThread)
    ActiveSensorKey
    ActiveSensor
  where
  lenses (downstreamThreadID, downstreamThread) =
    M.singleton
      (DownstreamThreadKey downstreamThreadID)
      (ScopedLens (_2 . lens getter setter))
    where
      getter (DownstreamThread _maxValue ratelimit dtLastRef lastRead) =
        ActiveSensor
          { activeMeta = SensorMeta
              { tags = [Maximize, DownstreamThreadSignal],
                range = 0 ... (maxValue downstreamThread & fromProgress & fromIntegral),
                lastReferenceMeasurements = dtLastRef,
                last = lastRead <&> fmap (fromIntegral . fromProgress),
                cumulative = IntervalBased
              },
            maxFrequency = ratelimit,
            process = identity
          }
      setter :: DownstreamThread -> ActiveSensor -> DownstreamThread
      setter dc activeSensor =
        dc &~ do
          #maxValue .= activeSensor ^. _meta . #range . to sup . to floor . to progress
          #dtLastReferenceMeasurements .= activeSensor ^. _meta . #lastReferenceMeasurements
          #lastRead .= over _Just (& _2 %~ progress . floor) (activeSensor ^. _meta . #last)
