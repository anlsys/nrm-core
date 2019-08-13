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
      { startContainerUUID :: C.ContainerUUID
      , startApplicationUUID :: A.ApplicationUUID
      }

newtype Exit
  = Exit
      { exitApplicationUUID :: A.ApplicationUUID
      }

data Performance
  = Performance
      { performanceContainerUUID :: C.ContainerUUID
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
      { container_uuid = C.toText startContainerUUID
      , application_uuid = A.toText startApplicationUUID
      }
