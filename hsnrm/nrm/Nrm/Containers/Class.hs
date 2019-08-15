{-|
Module      : Nrm.Containers.Class
Copyright   : (c) 2019, UChicago Argonne, LLC.
License     : BSD3
Maintainer  : fre@freux.fr

-}
module Nrm.Containers.Class
  ( ContainerRuntime (..)
  , ApplicationProcess (..)
  , AppStartConfig (..)
  )
where

import Data.MessagePack
import Nrm.Types.Container
import Nrm.Types.DownstreamClient
import Nrm.Types.Process as P
import Protolude

data ApplicationProcess
  = Registered DownstreamID P.ProcessID
  | Unregistered DownstreamID
  deriving (Eq, Show, Generic)

instance MessagePack ApplicationProcess

data AppStartConfig
  = AppStartConfig
      { command :: Command
      , arguments :: Arguments
      , applicationUUID :: DownstreamID
      }

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
    -> m (Either Text (runtime, ContainerID))

  doPrepareStartApp
    :: runtime
    -> ContainerID
    -> AppStartConfig
    -> m (Either Text (runtime, Command, Arguments))

  doStopContainer
    :: runtime
    -> ContainerID
    -> m (Either Text runtime)

  registerStartApp
    :: runtime
    -> ContainerID
    -> DownstreamID
    -> P.ProcessID
    -> runtime

  registerStopApp
    :: runtime
    -> Either P.ProcessID DownstreamID
    -> runtime

  listApplications
    :: runtime
    -> ContainerID
    -> Maybe [ApplicationProcess]

  listContainers
    :: runtime
    -> [ContainerID]
