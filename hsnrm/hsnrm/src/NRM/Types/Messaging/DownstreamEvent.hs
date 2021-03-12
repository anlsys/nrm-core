{-# LANGUAGE DerivingVia #-}
{-# OPTIONS_GHC -fno-warn-partial-fields #-}

-- |
-- Module      : NRM.Types.Messaging.DownstreamEvent
-- Copyright   : (c) UChicago Argonne, 2019
-- License     : BSD3
-- Maintainer  : fre@freux.fr
--
-- This module implements NRM's Downstream messaging API, which is used
-- for reporting application performance and progress.
module NRM.Types.Messaging.DownstreamEvent
  ( Event (..),
    PhaseContext (..),
  )
where

import Data.Aeson
import Data.JSON.Schema
import Data.MessagePack
import NRM.Classes.Messaging
import NRM.Types.CmdID as CmdID
import NRM.Types.DownstreamThreadID
import NRM.Types.Units
import Protolude

-- | Partial record selectors are unfortunately used here.
-- They make the top level of the serialized JSON message format
-- more readable by embedding tags.
data Event
  = -- | Performance wrapping operation count report.
    CmdPerformance
      { cmdID :: CmdID,
        perf :: Operations,
        timestamp :: Double
      }
  | -- | Pausing performance wrapping operation reports.
    CmdPause
      { cmdID :: CmdID
      }
  | -- | Instrumented thread progress report.
    ThreadProgress
      { downstreamThreadID :: DownstreamThreadID,
        progress :: Progress
      }
  | -- | Pausing instrumented thread progress reports.
    ThreadPause
      { downstreamThreadID :: DownstreamThreadID
      }
  | -- | Preloaded MPI progress report.
    ThreadPhaseContext
      { downstreamThreadID :: DownstreamThreadID,
        phaseContext :: PhaseContext
      }
  | -- | Pausing preloaded MPI progress report.
    ThreadPhasePause
      { downstreamThreadID :: DownstreamThreadID
      }
  deriving (Generic, MessagePack, NRMMessage)

data PhaseContext
  = PhaseContext
      { cpu :: Int,
        aggregation :: Int,
        computetime :: Int,
        totaltime :: Int
      }
  deriving (Generic, Show, MessagePack)
  deriving (JSONSchema, ToJSON, FromJSON) via GenericJSON PhaseContext
