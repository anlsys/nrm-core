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
import qualified Nrm.Types.Application as A
import qualified Nrm.Types.Container as C
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
      , startApplicationUUID :: A.ApplicationUUID
      }

newtype Exit
  = Exit
      { exitApplicationUUID :: A.ApplicationUUID
      }

data Performance
  = Performance
      { performanceContainerID :: C.ContainerID
      , performanceApplicationUUID :: A.ApplicationUUID
      , perf :: U.Operations
      }

data Progress
  = Progress
      { progressApplicationUUID :: A.ApplicationUUID
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
      , application_uuid = A.toText startApplicationUUID
      }
    EventExit Exit {..} -> J.Exit
      { application_uuid = A.toText exitApplicationUUID
      }
    EventPerformance Performance {..} -> J.Performance
      { container_uuid = C.toText performanceContainerID
      , application_uuid = A.toText performanceApplicationUUID
      , perf = o
      }
      where
        (U.Operations o) = perf
    EventProgress Progress {..} -> J.Progress
      { application_uuid = A.toText progressApplicationUUID
      , payload = p
      }
      where
        (U.Progress p) = payload
    EventPhaseContext PhaseContext {..} -> J.PhaseContext {..}

  fromJ = \case
    J.Start {..} ->
      EventStart $ Start
        { startContainerID = C.parseContainerID container_uuid
        , startApplicationUUID = fromMaybe
          (panic "DownstreamEvent fromJ error on Application UUID")
          (A.parseApplicationUUID application_uuid)
        }
    J.Exit {..} ->
      EventExit $ Exit
        { exitApplicationUUID = fromMaybe
            (panic "DownstreamEvent fromJ error on Application UUID")
            (A.parseApplicationUUID application_uuid)
        }
    J.Performance {..} ->
      EventPerformance $ Performance
        { performanceContainerID = C.parseContainerID container_uuid
        , performanceApplicationUUID = fromMaybe
          (panic "DownstreamEvent fromJ error on Application UUID")
          (A.parseApplicationUUID application_uuid)
        , perf = U.Operations perf
        }
    J.Progress {..} ->
      EventProgress $ Progress
        { progressApplicationUUID = fromMaybe
            (panic "DownstreamEvent fromJ error on Application UUID")
            (A.parseApplicationUUID application_uuid)
        , payload = U.Progress payload
        }
    J.PhaseContext {..} -> EventPhaseContext PhaseContext {..}
