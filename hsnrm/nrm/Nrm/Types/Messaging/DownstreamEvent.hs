{-|
Module      : Nrm.Types.Messaging.DownstreamEvent
Copyright   : (c) UChicago Argonne, 2019
License     : BSD3
Maintainer  : fre@freux.fr
-}
module Nrm.Types.Messaging.DownstreamEvent
  ( Event (..)
  )
where

import Nrm.Classes.Messaging
import qualified Nrm.Types.Application as A
import qualified Nrm.Types.Container as C
import qualified Nrm.Types.Messaging.DownstreamEvent.JSON as J
import qualified Nrm.Types.Units as U
import Protolude

data Event
  = Start
      { container_uuid :: C.ContainerUUID
      , application_uuid :: A.ApplicationUUID
      }
  | Exit
      { application_uuid :: A.ApplicationUUID
      }
  | Performance
      { container_uuid :: C.ContainerUUID
      , application_uuid :: A.ApplicationUUID
      , perf :: U.Operations
      }
  | Progress
      { application_uuid :: A.ApplicationUUID
      , payload :: U.Progress
      }
  | PhaseContext
      { cpu :: Int
      , startcompute :: Int
      , endcompute :: Int
      , startbarrier :: Int
      , endbarrier :: Int
      }

instance JSONLayer Event J.Event where
