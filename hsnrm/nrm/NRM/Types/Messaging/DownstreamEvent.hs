{-|
Module      : NRM.Types.Messaging.DownstreamEvent
Copyright   : (c) UChicago Argonne, 2019
License     : BSD3
Maintainer  : fre@freux.fr
-}
module NRM.Types.Messaging.DownstreamEvent
  ( Event (..)
  , CmdStart (..)
  , CmdPerformance (..)
  , CmdExit (..)
  , ThreadStart (..)
  , ThreadProgress (..)
  , ThreadPhaseContext (..)
  , ThreadExit (..)
  )
where

import Data.Maybe (fromJust)
import Data.MessagePack
import qualified NRM.Classes.Messaging as M
import qualified NRM.Types.Cmd as Cmd
import qualified NRM.Types.DownstreamThreadClient as D
import qualified NRM.Types.Messaging.DownstreamEvent.JSON as J
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
      { cmdStartCmdID :: Cmd.CmdID
      }
  deriving (Generic, MessagePack)

data CmdPerformance
  = CmdPerformance
      { cmdPerformanceCmdID :: Cmd.CmdID
      , perf :: U.Operations
      }
  deriving (Generic, MessagePack)

newtype CmdExit
  = CmdExit
      { cmdExitCmdID :: Cmd.CmdID
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
    EventCmdPerformance CmdPerformance {..} ->
      J.CmdPerformance
        { cmdID = Cmd.toText cmdPerformanceCmdID
        , perf = U.fromOps perf
        }
    _ -> panic "Non-Cmd downstream API not implemented yet."

  fromJ = \case
    J.CmdPerformance {..} ->
      EventCmdPerformance
        (CmdPerformance (fromJust $ Cmd.fromText cmdID) (U.Operations perf))
    _ -> panic "Non-Cmd downstream API not implemented yet."
