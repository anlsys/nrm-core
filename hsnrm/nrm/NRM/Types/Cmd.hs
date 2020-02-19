{-# LANGUAGE DerivingVia #-}
{-# LANGUAGE TypeApplications #-}

-- |
-- Module      : NRM.Types.Cmd
-- Copyright   : (c) UChicago Argonne, 2019
-- License     : BSD3
-- Maintainer  : fre@freux.fr
module NRM.Types.Cmd
  ( Cmd (..),
    CmdCore (..),
    CmdSpec (..),
    mkCmd,
    registerPID,
    TaskID (..),
    Command (..),
    Arguments (..),
    Arg (..),
    Env (..),
    wrapCmd,
    addDownstreamCmdClient,
    addDownstreamThreadClient,
    removeDownstreamCmdClient,
    removeDownstreamThreadClient,
  )
where

import Control.Lens
import Data.Aeson as A
import Data.Generics.Product
import Data.JSON.Schema
import Data.MessagePack
import Data.String (IsString (..))
import Dhall hiding (field)
import LMap.Map as LM
import LensMap.Core
import NRM.Classes.Messaging
import NRM.Orphans.ExitCode ()
import NRM.Orphans.UUID ()
import NRM.Types.CmdID
import NRM.Types.DownstreamCmd
import NRM.Types.DownstreamCmdID
import NRM.Types.DownstreamThread
import NRM.Types.DownstreamThreadID
import NRM.Types.Manifest as Manifest
import NRM.Types.MemBuffer as MemBuffer
import NRM.Types.Process
import NRM.Types.Sensor
import NRM.Types.Units
import qualified NRM.Types.UpstreamClient as UC
import Protolude

data CmdSpec
  = CmdSpec
      { cmd :: Command,
        args :: Arguments,
        env :: Env
      }
  deriving (Show, Generic, MessagePack)
  deriving (JSONSchema, ToJSON, FromJSON) via GenericJSON CmdSpec

data CmdCore
  = CmdCore
      { cmdPath :: Command,
        arguments :: Arguments,
        upstreamClientID :: Maybe UC.UpstreamClientID,
        manifest :: Manifest
      }
  deriving (Show, Generic, MessagePack)
  deriving (JSONSchema, ToJSON, FromJSON) via GenericJSON CmdCore

data Cmd
  = Cmd
      { cmdCore :: CmdCore,
        pid :: ProcessID,
        processState :: ProcessState,
        downstreamCmds :: LM.Map DownstreamCmdID DownstreamCmd,
        downstreamThreads :: LM.Map DownstreamThreadID DownstreamThread
      }
  deriving (Show, Generic, MessagePack)
  deriving (JSONSchema, ToJSON, FromJSON) via GenericJSON Cmd

mkCmd :: CmdSpec -> Manifest -> Maybe UC.UpstreamClientID -> CmdCore
mkCmd s manifest clientID = CmdCore {cmdPath = cmd s, arguments = args s, upstreamClientID = clientID, ..}

registerPID :: CmdCore -> ProcessID -> Cmd
registerPID c pid = Cmd
  { cmdCore = c,
    processState = blankState,
    downstreamCmds = LM.empty,
    downstreamThreads = LM.empty,
    ..
  }

addDownstreamCmdClient ::
  Cmd ->
  DownstreamCmdID ->
  Maybe Cmd
addDownstreamCmdClient Cmd {..} downstreamCmdClientID =
  cmdCore & manifest & Manifest.app & Manifest.perfwrapper & \case
    PerfwrapperDisabled -> Nothing
    Perfwrapper perfFreq perfLimit ->
      Just $ Cmd
        { downstreamCmds =
            LM.insert
              downstreamCmdClientID
              DownstreamCmd
                { maxValue = perfLimit,
                  ratelimit = perfFreq,
                  dtLastReferenceMeasurements = MemBuffer.empty,
                  lastRead = Nothing
                }
              downstreamCmds,
          ..
        }

addDownstreamThreadClient ::
  Cmd ->
  DownstreamThreadID ->
  Maybe Cmd
addDownstreamThreadClient Cmd {..} downstreamThreadClientID =
  cmdCore & manifest & Manifest.app & Manifest.instrumentation <&> \(Manifest.Instrumentation ratelimit) ->
    Cmd
      { downstreamThreads =
          LM.insert
            downstreamThreadClientID
            DownstreamThread
              { maxValue = 1 & progress,
                ratelimit = ratelimit,
                dtLastReferenceMeasurements = MemBuffer.empty,
                lastRead = Nothing
              }
            downstreamThreads,
        ..
      }

removeDownstreamThreadClient :: Cmd -> DownstreamThreadID -> Cmd
removeDownstreamThreadClient Cmd {..} downstreamThreadClientID = Cmd
  { downstreamThreads = LM.delete downstreamThreadClientID downstreamThreads,
    ..
  }

removeDownstreamCmdClient :: Cmd -> DownstreamCmdID -> Cmd
removeDownstreamCmdClient Cmd {..} downstreamCmdClientID = Cmd
  { downstreamCmds = LM.delete downstreamCmdClientID downstreamCmds,
    ..
  }

newtype Arg = Arg Text
  deriving (Show, Generic, MessagePack)
  deriving (JSONSchema, ToJSON, FromJSON) via GenericJSON Arg

instance StringConv Arg Text where
  strConv _ (Arg x) = toS x

newtype Command = Command Text
  deriving (Show, Eq, Generic, MessagePack)
  deriving (JSONSchema, ToJSON, FromJSON) via GenericJSON Command
  deriving (IsString, Interpret, Inject) via Text

instance StringConv Command Text where
  strConv _ (Command x) = toS x

newtype Arguments = Arguments [Arg]
  deriving (Show, Generic, MessagePack)
  deriving (JSONSchema, ToJSON, FromJSON) via GenericJSON Arguments

newtype Env = Env {fromEnv :: LM.Map Text Text}
  deriving (Show, Generic, MessagePack)
  deriving (JSONSchema, ToJSON, FromJSON) via GenericJSON Env
  deriving (Semigroup, Monoid) via LM.Map Text Text

wrapCmd :: Command -> (Command, Arguments) -> (Command, Arguments)
wrapCmd c (Command a, Arguments as) = (c, Arguments $ Arg a : as)

instance HasLensMap (CmdID, Cmd) ActiveSensorKey ActiveSensor where
  lenses (_cmdID, cmd) =
    (addPath (_2 . field @"downstreamCmds") <$> lenses (downstreamCmds cmd))
      <> (addPath (_2 . field @"downstreamThreads") <$> lenses (downstreamThreads cmd))
