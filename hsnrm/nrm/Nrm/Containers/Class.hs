{-|
Module      : Nrm.Containers.Class
Copyright   : (c) 2019, UChicago Argonne, LLC.
License     : BSD3
Maintainer  : fre@freux.fr

-}
module Nrm.Containers.Class
  ( ContainerRuntime (..)
  , ApplicationProcess (..)
  )
where

import Data.MessagePack
import Nrm.Types.Application
import Nrm.Types.Container
import Nrm.Types.Process as P
import Protolude

data ApplicationProcess
  = Registered ApplicationUUID P.ProcessID
  | Unregistered ApplicationUUID
  deriving (Eq, Show, Generic)

instance MessagePack ApplicationProcess

class
  (MonadIO m)
  => ContainerRuntime m runtime containerconfig runtimeconfig
    | runtime -> containerconfig
    , runtime -> runtimeconfig where

  doEnableRuntime :: runtimeConfig -> m (Either Text runtime)

  doDisableRuntime
    :: runtime
    -> m (Either Text runtime)

  doCreateContainer
    :: runtime
    -> containerconfig
    -> m (Either Text (runtime, ContainerUUID))

  doPrepareStartApp
    :: runtime
    -> ContainerUUID
    -> AppStartConfig
    -> m (Either Text (runtime, Command, Arguments))

  doStopContainer
    :: runtime
    -> ContainerUUID
    -> m (Either Text runtime)

  registerStartApp
    :: runtime
    -> ContainerUUID
    -> ApplicationUUID
    -> P.ProcessID
    -> runtime

  registerStopApp
    :: runtime
    -> Either P.ProcessID ApplicationUUID
    -> runtime

  listApplications
    :: runtime
    -> ContainerUUID
    -> Maybe [ApplicationProcess]

  listContainers
    :: runtime
    -> [ContainerUUID]
