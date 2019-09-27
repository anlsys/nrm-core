{-|
Module      : export
Copyright   : (c) 2019, UChicago Argonne, LLC.
License     : BSD3
Maintainer  : fre@freux.fr
-}
module NRM.Export
  ( -- * CLI interfaces
    parseDaemon
  , -- * Configuration queries
    isDebug
  , isVerbose
  , showConfiguration
  , C.logfile
  , upstreamPubAddress
  , upstreamRpcAddress
  , downstreamEventAddress
  , -- * State
    S.initialState
  , showState
  , -- * Event to behavior entry points
    downstreamReceive
  , upstreamReceive
  , doStdout
  , doStderr
  , doSensor
  , doControl
  , childDied
  , doShutdown
  , registerCmdSuccess
  , registerCmdFailure
  )
where

import qualified NRM.Behavior as B
import qualified NRM.Classes.Messaging as M
import qualified NRM.Optparse as O (parseArgDaemonCli)
import qualified NRM.State as S
{-import qualified NRM.Types.Slice as Ct-}
import qualified NRM.Types.Cmd as Cmd
import qualified NRM.Types.Configuration as C
  ( Cfg (..)
  , DaemonVerbosity (..)
  , DownstreamCfg (..)
  , UpstreamCfg (..)
  , logfile
  , verbose
  )
import qualified NRM.Types.DownstreamCmd as DC
import qualified NRM.Types.Messaging.UpstreamRep as URep
import qualified NRM.Types.Process as Process
import qualified NRM.Types.State as TS
import qualified NRM.Types.UpstreamClient as UC
import Protolude hiding (stderr, stdout)
import qualified System.Posix.Types as SPT
import Text.Pretty.Simple

-- | Parses Daemon CLI arguments
parseDaemon :: [Text] -> IO C.Cfg
parseDaemon = O.parseArgDaemonCli

-- | Queries configuration for 'verbose' verbosity
isVerbose :: C.Cfg -> Bool
isVerbose c =
  C.Verbose == C.verbose c

-- | Queries configuration for 'debug' verbosity
isDebug :: C.Cfg -> Bool
isDebug c =
  C.Debug == C.verbose c

-- | Show the configuration in text format
showConfiguration :: C.Cfg -> Text
showConfiguration = toS . pShow

-- | Query full upstream pub zmq address from configuration
upstreamPubAddress :: C.Cfg -> Text
upstreamPubAddress = buildAddress C.pubPort

-- | Query full upstream rpc zmq address from configuration
upstreamRpcAddress :: C.Cfg -> Text
upstreamRpcAddress = buildAddress C.rpcPort

-- | Query full downstream event address from configuration
downstreamEventAddress :: C.Cfg -> Text
downstreamEventAddress = C.downstreamBindAddress . C.downstreamCfg

buildAddress :: Show a => (C.UpstreamCfg -> a) -> C.Cfg -> Text
buildAddress accessor c = "tcp://" <> C.upstreamBindAddress u <> ":" <> show (accessor u)
  where
    u = C.upstreamCfg c

-- | Show the state in text format
showState :: TS.NRMState -> Text
showState = toS . pShow

-- | Behave on downstream message
downstreamReceive :: C.Cfg -> TS.NRMState -> Text -> Text -> IO (TS.NRMState, B.Behavior)
downstreamReceive cfg s msg clientid =
  B.behavior cfg s $
    B.DownstreamEvent
      ( fromMaybe
        (panic "couldn't parse downstream client ID")
        (DC.fromText clientid)
      )
      ( fromMaybe
        (panic "couldn't decode downstream rcv")
        (M.decodeT $ toS msg)
      )

-- | Behave on upstream message
upstreamReceive :: C.Cfg -> TS.NRMState -> Text -> Text -> IO (TS.NRMState, B.Behavior)
upstreamReceive cfg s msg clientid =
  B.behavior cfg s $
    B.Req
      (fromMaybe (panic "couldn't parse upstream client ID") (UC.fromText clientid))
      (fromMaybe (panic "couldn't decode downstream rcv") (M.decodeT msg))

-- | when it's time to activate a sensor
doSensor :: C.Cfg -> TS.NRMState -> IO (TS.NRMState, B.Behavior)
doSensor c s = B.behavior c s B.DoSensor

-- | when it's time to run the control loop
doControl :: C.Cfg -> TS.NRMState -> IO (TS.NRMState, B.Behavior)
doControl c s = B.behavior c s B.DoControl

-- | when a child dies
childDied :: C.Cfg -> TS.NRMState -> Int -> Int -> IO (TS.NRMState, B.Behavior)
childDied c s pid status =
  B.behavior c s
    ( B.ChildDied (Process.ProcessID . SPT.CPid $ fromIntegral pid)
      (if status == 0 then ExitSuccess else ExitFailure status)
    )

-- | when the runtime is shutdown
doShutdown :: C.Cfg -> TS.NRMState -> IO (TS.NRMState, B.Behavior)
doShutdown c s = B.behavior c s B.DoShutdown

-- | when a child produces something on its stdout
doStdout :: C.Cfg -> TS.NRMState -> Text -> Text -> IO (TS.NRMState, B.Behavior)
doStdout = handleTag URep.StdoutOutput

-- | when a child produces something on its stderr
doStderr :: C.Cfg -> TS.NRMState -> Text -> Text -> IO (TS.NRMState, B.Behavior)
doStderr = handleTag URep.StderrOutput

-- | when a command was properly started.
registerCmdSuccess :: C.Cfg -> TS.NRMState -> Text -> Int -> IO (TS.NRMState, B.Behavior)
registerCmdSuccess cfg s cmdIDT pid =
  B.behavior
    cfg
    s
    ( B.RegisterCmd
      (fromMaybe (panic "couldn't decode cmdID") (Cmd.fromText cmdIDT))
      (B.Launched (Process.ProcessID $ SPT.CPid $ fromIntegral pid))
    )

-- | when a command failed even starting.
registerCmdFailure :: C.Cfg -> TS.NRMState -> Text -> IO (TS.NRMState, B.Behavior)
registerCmdFailure cfg s cmdIDT =
  B.behavior
    cfg
    s
    ( B.RegisterCmd
      (fromMaybe (panic "couldn't decode cmdID") (Cmd.fromText cmdIDT))
      B.NotLaunched
    )

-- Utilities
handleTag :: URep.OutputType -> C.Cfg -> TS.NRMState -> Text -> Text -> IO (TS.NRMState, B.Behavior)
handleTag tag c s cmdIDT msg =
  B.behavior c s
    ( B.DoOutput
      (fromMaybe (panic "couldn't decode cmdID") (Cmd.fromText cmdIDT))
      tag
      msg
    )
