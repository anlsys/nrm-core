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

import Data.Aeson
import Data.MessagePack
import Nrm.Types.Container
import Nrm.Types.Process as P
import Protolude

data ApplicationProcess
  = Registered P.CmdID P.ProcessID
  | Unregistered P.CmdID
  deriving (Ord, Eq, Show, Generic, MessagePack, ToJSON, FromJSON)

data AppStartConfig
  = AppStartConfig
      { command :: Command
      , arguments :: Arguments
      , cmdID :: P.CmdID
      }
  deriving (Show, Generic, MessagePack, ToJSON, FromJSON)

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
    -> P.CmdID
    -> P.ProcessID
    -> runtime

  registerStopApp
    :: runtime
    -> Either P.ProcessID P.CmdID
    -> runtime

  listApplications
    :: runtime
    -> ContainerID
    -> Maybe [ApplicationProcess]

  listContainers
    :: runtime
    -> [ContainerID]
