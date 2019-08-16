{-|
Module      : Nrm.Types.Messaging.DownstreamEvent
Copyright   : (c) UChicago Argonne, 2019
License     : BSD3
Maintainer  : fre@freux.fr
-}
module Nrm.Types.Messaging.DownstreamEvent
  ( Event (..)
  , Start (..)
  , Exit (..)
  , Performance (..)
  , Progress (..)
  , PhaseContext (..)
  )
where

import qualified Nrm.Classes.Messaging as M
import qualified Nrm.Types.Container as C
import qualified Nrm.Types.DownstreamClient as D
import qualified Nrm.Types.Messaging.DownstreamEvent.JSON as J
import qualified Nrm.Types.Units as U
import Protolude

data Event
  = EventStart Start
  | EventExit Exit
  | EventPerformance Performance
  | EventProgress Progress
  | EventPhaseContext PhaseContext

data Start
  = Start
      { startContainerID :: C.ContainerID
      , startProcessID :: P.ProcessID
      , startThreadID :: P.ThreadID
      , startTaskID :: P.TaskID
      }

newtype Exit
  = Exit
      { exitDownstreamID :: D.DownstreamID
      }

data Performance
  = Performance
      { performanceContainerID :: C.ContainerID
      , startProcessID :: P.ProcessID
      , startThreadID :: P.ThreadID
      , startTaskID :: P.TaskID
      , perf :: U.Operations
      }

data Progress
  = Progress
      { startProcessID :: P.ProcessID
      , startThreadID :: P.ThreadID
      , startTaskID :: P.TaskID
      , payload :: U.Progress
      }

data PhaseContext
  = PhaseContext
      { cpu :: Int
      , startcompute :: Int
      , endcompute :: Int
      , startbarrier :: Int
      , endbarrier :: Int
      }

instance M.NrmMessage Event J.Event where

  toJ = \case
    EventStart Start {..} -> J.Start
      { container_uuid = C.toText startContainerID
      , application_uuid = D.toText startDownstreamID
      }
    EventExit Exit {..} -> J.Exit
      { application_uuid = D.toText exitDownstreamID
      }
    EventPerformance Performance {..} -> J.Performance
      { container_uuid = C.toText performanceContainerID
      , application_uuid = D.toText performanceDownstreamID
      , perf = o
      }
      where
        (U.Operations o) = perf
    EventProgress Progress {..} -> J.Progress
      { application_uuid = D.toText progressDownstreamID
      , payload = p
      }
      where
        (U.Progress p) = payload
    EventPhaseContext PhaseContext {..} -> J.PhaseContext {..}

  fromJ = \case
    J.Start {..} ->
      EventStart $ Start
        { startContainerID = C.parseContainerID container_uuid
        , startDownstreamID = fromMaybe
          (panic "DownstreamEvent fromJ error on Application UUID")
          (D.parseDownstreamID application_uuid)
        }
    J.Exit {..} ->
      EventExit $ Exit
        { exitDownstreamID = fromMaybe
            (panic "DownstreamEvent fromJ error on Application UUID")
            (D.parseDownstreamID application_uuid)
        }
    J.Performance {..} ->
      EventPerformance $ Performance
        { performanceContainerID = C.parseContainerID container_uuid
        , performanceDownstreamID = fromMaybe
          (panic "DownstreamEvent fromJ error on Application UUID")
          (D.parseDownstreamID application_uuid)
        , perf = U.Operations perf
        }
    J.Progress {..} ->
      EventProgress $ Progress
        { progressDownstreamID = fromMaybe
            (panic "DownstreamEvent fromJ error on Application UUID")
            (D.parseDownstreamID application_uuid)
        , payload = U.Progress payload
        }
    J.PhaseContext {..} -> EventPhaseContext PhaseContext {..}
