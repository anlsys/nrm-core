{-# LANGUAGE DerivingVia #-}

-- |
-- Module      : NRM.Types.Behavior
-- Copyright   : (c) UChicago Argonne, 2019
-- License     : BSD3
-- Maintainer  : fre@freux.fr
module NRM.Types.Behavior
  ( NRMEvent (..),

    -- * The Behavior specification
    Behavior (..),
    CmdStatus (..),
  )
where

import Data.MessagePack
import qualified NRM.Classes.Messaging as M
import NRM.Types.Cmd
import NRM.Types.CmdID
import NRM.Types.Configuration
import NRM.Types.DownstreamClient
import NRM.Types.Messaging.DownstreamEvent
import NRM.Types.Messaging.UpstreamPub as UPub
import NRM.Types.Messaging.UpstreamRep
import NRM.Types.Messaging.UpstreamReq
import NRM.Types.Process
import NRM.Types.UpstreamClient
import Protolude hiding (Rep)

-- | The Behavior datatype describes an event from the runtime on which to react.
data NRMEvent
  = -- | A Request was received on the Upstream API.
    Req UpstreamClientID Req
  | -- | Registering a child process.
    RegisterCmd CmdID CmdStatus
  | -- | Event from the application side.
    DownstreamEvent DownstreamClientID Event
  | -- | Stdin/stdout data from the app side.
    DoOutput CmdID OutputType Text
  | -- | Child death event
    ChildDied ProcessID ExitCode
  | -- | Sensor callback
    DoSensor
  | -- | Control loop calback
    DoControl
  | -- | Shutting down the daemon
    DoShutdown

-- | The Launch status of a command, for registration.
data CmdStatus
  = -- | In case the command to start a child succeeded, mark it as registered and provide its PID.
    Launched ProcessID
  | -- | In case the command to start a child failed.
    NotLaunched
  deriving (Generic, MessagePack)

-- | The Behavior datatype encodes a behavior to be executed by the NRM runtime.
data Behavior
  = -- | Log a message
    Log DaemonVerbosity Text
  | -- | Reply to an upstream client.
    Rep UpstreamClientID Rep
  | -- | Publish messages on upstream
    Pub UPub.Pub
  | -- | Start a child process
    StartChild CmdID Command Arguments Env
  | -- | Kill children processes and send some messages back upstream.
    KillChildren [CmdID] [(UpstreamClientID, Rep)]
  | -- | Pop one child process and may send a message back upstream.
    ClearChild CmdID (Maybe (UpstreamClientID, Rep))
  deriving (Show, Generic)

-- | The sensitive unpacking that has to be pattern-matched on the python side.
-- These toObject/fromObject functions do not correspond to each other and the instance
-- just exists for passing the behavior to the python runtime.
instance MessagePack Behavior where

  toObject (Log Error msg) = toObject ("logError" :: Text, msg)
  toObject (Log Info msg) = toObject ("logInfo" :: Text, msg)
  toObject (Log Debug msg) = toObject ("logDebug" :: Text, msg)
  toObject (Pub msg) = toObject ("publish" :: Text, M.encodeT msg)
  toObject (Rep clientid msg) =
    toObject ("reply" :: Text, clientid, M.encodeT msg)
  toObject (StartChild cmdID cmd args env) =
    toObject ("cmd" :: Text, cmdID, cmd, args, env)
  toObject (KillChildren cmdIDs reps) =
    toObject
      ( "kill" :: Text,
        cmdIDs,
        second M.encodeT <$> reps
      )
  toObject (ClearChild cmdID maybeRep) =
    toObject
      ( "pop" :: Text,
        cmdID,
        second M.encodeT <$> Protolude.toList maybeRep
      )

  fromObject x = to <$> gFromObject x
