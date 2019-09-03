{-|
Module      : NRM.Types.Messaging.DownstreamEvent
Copyright   : (c) UChicago Argonne, 2019
License     : BSD3
Maintainer  : fre@freux.fr
-}
module NRM.Types.Messaging.DownstreamEvent
  ( Event (..)
  , CmdStart
  , CmdPerformance
  , CmdExit
  , ThreadStart
  , ThreadProgress
  , ThreadPhaseContext
  , ThreadExit
  )
where

import Data.Maybe (fromJust)
import Data.MessagePack
import qualified NRM.Classes.Messaging as M
import qualified NRM.Types.DownstreamClient as D
import qualified NRM.Types.Messaging.DownstreamEvent.JSON as J
import qualified NRM.Types.Process as P
import qualified NRM.Types.Units as U
import Protolude

data Event
  = EventCmdStart CmdStart
  | EventCmdPerformance CmdPerformance
  | EventCmdExit CmdExit
  | EventThreadStart ThreadStart
  | EventThreadProgress ThreadProgress
  | EventThreadPhaseContext ThreadPhaseContext
  | EventThreadExit ThreadExit
  deriving (Generic, MessagePack)

newtype ThreadStart
  = ThreadSTart
      { startDownstreamThreadID :: D.DownstreamThreadID
      }
  deriving (Generic, MessagePack)

data ThreadProgress
  = ThreadProgress
      { progressDownstreamThreadID :: D.DownstreamThreadID
      , progress :: U.Progress
      }
  deriving (Generic, MessagePack)

data ThreadPhaseContext
  = ThreadPhaseContext
      { threadPhaseContext :: D.DownstreamThreadID
      , phaseContext :: PhaseContext
      }
  deriving (Generic, MessagePack)

newtype ThreadExit
  = ThreadExit
      { exitDownstreamThreadId :: D.DownstreamThreadID
      }
  deriving (Generic, MessagePack)

newtype CmdStart
  = CmdStart
      { cmdStartCmdID :: P.CmdID
      }
  deriving (Generic, MessagePack)

data CmdPerformance
  = CmdPerformance
      { cmdPerformanceCmdID :: P.CmdID
      , perf :: U.Operations
      }
  deriving (Generic, MessagePack)

newtype CmdExit
  = CmdExit
      { cmdExitCmdID :: P.CmdID
      }
  deriving (Generic, MessagePack)

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
    EventCmdStart CmdStart {..} ->
      J.CmdStart $ P.toText cmdStartCmdID
    EventCmdPerformance CmdPerformance {..} ->
      J.CmdPerformance
        { cmdID = P.toText cmdPerformanceCmdID
        , perf = U.ops perf
        }
    EventCmdExit CmdExit {..} ->
      J.CmdExit $ P.toText cmdExitCmdID
    _ -> panic "Non-Cmd downstream API not implemented yet."

  fromJ = \case
    J.CmdStart {..} -> EventCmdStart (CmdStart $ fromJust $ P.fromText cmdID)
    J.CmdExit {..} -> EventCmdExit (CmdExit $ fromJust $ P.fromText cmdID)
    J.CmdPerformance {..} ->
      EventCmdPerformance
        (CmdPerformance (fromJust $ P.fromText cmdID) (U.Operations perf))
    _ -> panic "Non-Cmd downstream API not implemented yet."
