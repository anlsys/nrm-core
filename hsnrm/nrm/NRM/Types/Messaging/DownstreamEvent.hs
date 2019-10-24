{-# LANGUAGE DerivingVia #-}

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
import Data.Maybe (fromJust)
import Data.MessagePack
import NRM.Classes.Messaging
import qualified NRM.Classes.Messaging as M
import NRM.Types.CmdID as CmdID
import NRM.Types.DownstreamThreadID
import qualified NRM.Types.Messaging.DownstreamEvent.JSON as J
import qualified NRM.Types.Process as P
import NRM.Types.Units
import Protolude

data Event
  = CmdPerformance CmdID Operations
  | CmdPause CmdID
  | ThreadProgress DownstreamThreadID Progress
  | ThreadPause DownstreamThreadID
  | ThreadPhaseContext DownstreamThreadID PhaseContext
  | ThreadPhasePause DownstreamThreadID
  deriving (Generic, MessagePack)

data PhaseContext
  = PhaseContext
      { cpu :: Int,
        aggregation :: Int,
        computetime :: Int,
        totaltime :: Int
      }
  deriving (Generic, Show, MessagePack)
  deriving (JSONSchema, ToJSON, FromJSON) via GenericJSON PhaseContext

instance M.NRMMessage Event J.Event where

  toJ = \case
    CmdPerformance cmdID perf ->
      J.CmdPerformance
        { cmdID = CmdID.toText cmdID,
          perf = fromOps perf
        }
    CmdPause cmdID ->
      J.CmdPause
        { cmdID = CmdID.toText cmdID
        }
    ThreadProgress
      (DownstreamThreadID threadCmdID processID taskID threadID)
      threadProgress ->
        J.ThreadProgress
          { cmdID = CmdID.toText threadCmdID,
            processID = fromIntegral $ P.rawPid processID,
            taskID = fromTaskID taskID,
            threadID = fromIntegral threadID,
            payload = fromProgress threadProgress
          }
    ThreadPause (DownstreamThreadID threadCmdID processID taskID threadID) ->
      J.ThreadPause
        { cmdID = CmdID.toText threadCmdID,
          processID = fromIntegral processID,
          taskID = fromTaskID taskID,
          threadID = fromIntegral threadID
        }
    ThreadPhaseContext
      (DownstreamThreadID threadCmdID processID taskID threadID)
      (PhaseContext cpu aggregation computetime totaltime) ->
        J.ThreadPhaseContext
          { cmdID = CmdID.toText threadCmdID,
            processID = fromIntegral processID,
            taskID = fromTaskID taskID,
            threadID = fromIntegral threadID,
            cpu = cpu,
            aggregation = aggregation,
            computetime = computetime,
            totaltime = totaltime
          }
    ThreadPhasePause (DownstreamThreadID threadCmdID processID taskID threadID) ->
      J.ThreadPhasePause
        { cmdID = CmdID.toText threadCmdID,
          processID = fromIntegral processID,
          taskID = fromTaskID taskID,
          threadID = fromIntegral threadID
        }

  fromJ = \case
    J.CmdPerformance {..} ->
      CmdPerformance (fromJust $ fromText cmdID) (Operations perf)
    J.CmdPause {..} ->
      CmdPause (fromJust $ CmdID.fromText cmdID)
    J.ThreadProgress {..} ->
      ThreadProgress
        ( DownstreamThreadID
            (fromJust $ CmdID.fromText cmdID)
            (fromInteger $ toInteger processID)
            (TaskID taskID)
            (fromInteger $ toInteger threadID)
        )
        (payload & progress)
    J.ThreadPause {..} ->
      ThreadPause
        ( DownstreamThreadID
            (fromJust $ CmdID.fromText cmdID)
            (fromInteger $ toInteger processID)
            (TaskID taskID)
            (fromInteger $ toInteger threadID)
        )
    J.ThreadPhaseContext {..} ->
      ThreadPhaseContext
        ( DownstreamThreadID
            (fromJust $ CmdID.fromText cmdID)
            (fromInteger $ toInteger processID)
            (TaskID taskID)
            (fromInteger $ toInteger threadID)
        )
        (PhaseContext cpu aggregation computetime totaltime)
    J.ThreadPhasePause {..} ->
      ThreadPhasePause
        ( DownstreamThreadID
            (fromJust $ CmdID.fromText cmdID)
            (fromInteger $ toInteger processID)
            (TaskID taskID)
            (fromInteger $ toInteger threadID)
        )
