{-# LANGUAGE AllowAmbiguousTypes #-}
{-# LANGUAGE FunctionalDependencies #-}
{-# LANGUAGE NoImplicitPrelude #-}

{-|
Module      : Nrm.Containers.Class
Description : container runtime interface
Copyright   : (c) 2019, UChicago Argonne, LLC.
License     : BSD3
Maintainer  : fre@freux.fr

-}
module Nrm.Containers.Class
  ( ContainerRuntime (..)
  , ApplicationProcess (..)
  )
where

import Nrm.Types.Application
import Nrm.Types.Container
import Protolude
import System.Posix.Types

data ApplicationProcess
  = Registered ApplicationUUID ProcessID
  | Unregistered ApplicationUUID
  deriving (Eq)

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
    -> ProcessID
    -> runtime

  registerStopApp
    :: runtime
    -> Either ProcessID ApplicationUUID
    -> runtime

  listApplications
    :: runtime
    -> ContainerUUID
    -> Maybe [ApplicationProcess]

  listContainers
    :: runtime
    -> [ContainerUUID]
