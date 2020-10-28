{-# LANGUAGE DerivingVia #-}

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
import Data.Generics.Labels ()
import Data.JSON.Schema
import Data.Map as M
import Data.MessagePack
import LensMap.Core
import NRM.Classes.Messaging
import qualified NRM.Types.Configuration as Cfg
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
        dtLastReferenceMeasurements :: MemBuffer,
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
    M.singleton
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
                cumulative = Cfg.IntervalBased
              },
            maxFrequency = ratelimit,
            process = identity
          }
      setter :: DownstreamCmd -> ActiveSensor -> DownstreamCmd
      setter dc activeSensor =
        dc &~ do
          #maxValue .= activeSensor ^. _meta . #range . to sup . to floor . to Operations
          #dtLastReferenceMeasurements .= activeSensor ^. _meta . to lastReferenceMeasurements
          #lastRead .= over _Just (over _2 $ progress . floor) (activeSensor ^. _meta . #last)
