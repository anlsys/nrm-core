{-# LANGUAGE DerivingVia #-}

{-|
Module      : NRM.Types.Messaging.DownstreamEvent
Copyright   : (c) UChicago Argonne, 2019
License     : BSD3
Maintainer  : fre@freux.fr
-}
module NRM.Types.Messaging.DownstreamEvent
  ( Event (..)
  , CmdHeader (..)
  , ThreadHeader (..)
  , Performance (..)
  , Progress (..)
  , PhaseContext (..)
  )
where

import Data.Aeson
import Data.JSON.Schema
import Data.Maybe (fromJust)
import Data.MessagePack
import NRM.Classes.Messaging
import qualified NRM.Classes.Messaging as M
import qualified NRM.Types.Cmd as Cmd
import NRM.Types.DownstreamThreadID
import qualified NRM.Types.Messaging.DownstreamEvent.JSON as J
import qualified NRM.Types.Process as P
import qualified NRM.Types.Units as U
import Protolude

data Event
  = CmdPerformance CmdHeader Performance
  | CmdPause CmdHeader
  | ThreadProgress ThreadHeader Progress
  | ThreadPause ThreadHeader
  | ThreadPhaseContext ThreadHeader PhaseContext
  | ThreadPhasePause ThreadHeader
  deriving (Generic, MessagePack)

data CmdHeader
  = CmdHeader
      { cmdID :: Cmd.CmdID
      , timestamp :: U.Time
      }
  deriving (Show, Generic, MessagePack)
  deriving (JSONSchema, ToJSON, FromJSON) via GenericJSON CmdHeader

data ThreadHeader
  = ThreadHeader
      { threadCmdID :: Cmd.CmdID
      , processID :: P.ProcessID
      , taskID :: TaskID
      , threadID :: ThreadID
      }
  deriving (Generic, MessagePack)

newtype Progress
  = Progress
      { progress :: U.Progress
      }
  deriving (Show, Generic, MessagePack)
  deriving (JSONSchema, ToJSON, FromJSON) via GenericJSON Progress

newtype Performance
  = Performance
      { perf :: U.Operations
      }
  deriving (Show, Generic, MessagePack)
  deriving (JSONSchema, ToJSON, FromJSON) via GenericJSON Performance

data PhaseContext
  = PhaseContext
      { cpu :: Int
      , startcompute :: Int
      , endcompute :: Int
      , startbarrier :: Int
      , endbarrier :: Int
      }
  deriving (Generic, MessagePack)

instance M.NRMMessage Event J.Event where

  toJ = \case
    CmdPerformance (CmdHeader cmdID timestamp) Performance {..} ->
      J.CmdPerformance
        { cmdID = Cmd.toText cmdID
        , timestamp = U.fromSeconds timestamp
        , perf = U.fromOps perf
        }
    CmdPause (CmdHeader cmdID timestamp) ->
      J.CmdPause
        { cmdID = Cmd.toText cmdID
        , timestamp = U.fromSeconds timestamp
        }
    ThreadProgress
      (ThreadHeader threadCmdID processID taskID threadID)
      (Progress progress) ->
        J.ThreadProgress
          { cmdID = Cmd.toText threadCmdID
          , processID = fromIntegral $ P.rawPid processID
          , taskID = fromIntegral taskID
          , threadID = fromIntegral threadID
          , payload = U.fromProgress progress
          }
    ThreadPause (ThreadHeader threadCmdID processID taskID threadID) ->
      J.ThreadPause
        { cmdID = Cmd.toText threadCmdID
        , processID = fromIntegral processID
        , taskID = fromIntegral taskID
        , threadID = fromIntegral threadID
        }
    ThreadPhaseContext
      (ThreadHeader threadCmdID processID taskID threadID)
      (PhaseContext cpu startcompute endcompute startbarrier endbarrier) ->
        J.ThreadPhaseContext
          { cmdID = Cmd.toText threadCmdID
          , processID = fromIntegral processID
          , taskID = fromIntegral taskID
          , threadID = fromIntegral threadID
          , cpu = cpu
          , startcompute = startcompute
          , endcompute = endcompute
          , startbarrier = startbarrier
          , endbarrier = endbarrier
          }
    ThreadPhasePause (ThreadHeader threadCmdID processID taskID threadID) ->
      J.ThreadPhasePause
        { cmdID = Cmd.toText threadCmdID
        , processID = fromIntegral processID
        , taskID = fromIntegral taskID
        , threadID = fromIntegral threadID
        }

  fromJ = \case
    J.CmdPerformance {..} ->
      CmdPerformance (CmdHeader (fromJust $ Cmd.fromText cmdID) (timestamp & U.seconds)) (Performance $ U.Operations perf)
    J.CmdPause {..} ->
      CmdPause (CmdHeader (fromJust $ Cmd.fromText cmdID) (timestamp & U.seconds))
    J.ThreadProgress {..} ->
      ThreadProgress
        ( ThreadHeader
          (fromJust $ Cmd.fromText cmdID)
          (fromInteger $ toInteger processID)
          (fromInteger $ toInteger taskID)
          (fromInteger $ toInteger threadID)
        )
        (Progress (payload & U.progress))
    J.ThreadPause {..} ->
      ThreadPause
        ( ThreadHeader
          (fromJust $ Cmd.fromText cmdID)
          (fromInteger $ toInteger processID)
          (fromInteger $ toInteger taskID)
          (fromInteger $ toInteger threadID)
        )
    J.ThreadPhaseContext {..} ->
      ThreadPhaseContext
        ( ThreadHeader
          (fromJust $ Cmd.fromText cmdID)
          (fromInteger $ toInteger processID)
          (fromInteger $ toInteger taskID)
          (fromInteger $ toInteger threadID)
        )
        (PhaseContext cpu startcompute endcompute startbarrier endbarrier)
    J.ThreadPhasePause {..} ->
      ThreadPhasePause
        ( ThreadHeader
          (fromJust $ Cmd.fromText cmdID)
          (fromInteger $ toInteger processID)
          (fromInteger $ toInteger taskID)
          (fromInteger $ toInteger threadID)
        )
