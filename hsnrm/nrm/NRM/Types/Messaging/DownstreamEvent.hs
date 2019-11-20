{-# LANGUAGE DerivingVia #-}
{-# OPTIONS_GHC -fno-warn-partial-fields #-}

-- |
-- Module      : NRM.Types.Messaging.DownstreamEvent
-- Copyright   : (c) UChicago Argonne, 2019
-- License     : BSD3
-- Maintainer  : fre@freux.fr
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

-- | partial record selectors are unfortunately used here.
-- They make the top level of the serialized JSON message format
-- more readable by embedding tags.
data Event
  = -- | performance wrapping operation count report
    CmdPerformance
      { cmdID :: CmdID,
        perf :: Operations
      }
  | -- | pausing performance wrapping operation reports
    CmdPause
      { cmdID :: CmdID
      }
  | -- | instrumented thread progress report
    ThreadProgress
      { downstreamThreadID :: DownstreamThreadID,
        progress :: Progress
      }
  | -- | pausing instrumented thread progress reports
    ThreadPause
      { downstreamThreadId :: DownstreamThreadID
      }
  | -- | preloaded MPI progress report
    ThreadPhaseContext
      { downstreamThreadId :: DownstreamThreadID,
        phaseContext :: PhaseContext
      }
  | -- | pausing preloaded MPI progress report
    ThreadPhasePause
      { downstreamThreadId :: DownstreamThreadID
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
