{-|
Module      : NRM.Slices.Class
Copyright   : (c) 2019, UChicago Argonne, LLC.
License     : BSD3
Maintainer  : fre@freux.fr

-}
module NRM.Slices.Class
  ( SliceRuntime (..)
  , ApplicationProcess (..)
  , AppStartConfig (..)
  )
where

import Data.Aeson
import Data.MessagePack
import NRM.Types.Slice
import NRM.Types.Process as P
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
  => SliceRuntime m runtime sliceconfig runtimeconfig
    | runtime -> sliceconfig
    , runtime -> runtimeconfig where

  doEnableRuntime :: runtimeConfig -> m (Either Text runtime)

  doDisableRuntime
    :: runtime
    -> m (Either Text runtime)

  doCreateSlice
    :: runtime
    -> sliceconfig
    -> m (Either Text (runtime, SliceID))

  doPrepareStartApp
    :: runtime
    -> SliceID
    -> AppStartConfig
    -> m (Either Text (runtime, Command, Arguments))

  doStopSlice
    :: runtime
    -> SliceID
    -> m (Either Text runtime)

  registerStartApp
    :: runtime
    -> SliceID
    -> P.CmdID
    -> P.ProcessID
    -> runtime

  registerStopApp
    :: runtime
    -> Either P.ProcessID P.CmdID
    -> runtime

  listApplications
    :: runtime
    -> SliceID
    -> Maybe [ApplicationProcess]

  listSlices
    :: runtime
    -> [SliceID]
