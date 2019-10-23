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

import Control.Lens
import qualified NRM.Behavior as B
import qualified NRM.Classes.Messaging as M
import qualified NRM.Optparse as O (parseArgDaemonCli)
import qualified NRM.State as S
import qualified NRM.Types.Behavior as B
import qualified NRM.Types.CmdID as CmdID
import qualified NRM.Types.Configuration as C
  ( Cfg (..)
  , DaemonVerbosity (..)
  , DownstreamCfg (..)
  , UpstreamCfg (..)
  , logfile
  , verbose
  )
import qualified NRM.Types.DownstreamCmdID as DC
import qualified NRM.Types.Messaging.UpstreamRep as URep
import qualified NRM.Types.Process as Process
import qualified NRM.Types.State as TS
import NRM.Types.Units
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
downstreamReceive :: C.Cfg -> TS.NRMState -> Double -> Text -> Text -> IO (TS.NRMState, B.Behavior)
downstreamReceive cfg s t msg clientid =
  B.DownstreamEvent <$> (DC.fromText clientid) <*> (M.decodeT $ toS msg) & \case
    Nothing -> return (s, B.Log "couldn't decode downstream receive")
    Just ev -> B.behavior cfg s (t & seconds) ev

-- | Behave on upstream message
upstreamReceive :: C.Cfg -> TS.NRMState -> Double -> Text -> Text -> IO (TS.NRMState, B.Behavior)
upstreamReceive cfg s t msg clientid =
  B.Req <$> (UC.fromText clientid) <*> (M.decodeT msg) & \case
    Nothing -> return (s, B.Log "couldn't decode upstream receive")
    Just ev -> B.behavior cfg s (t & seconds) ev

-- | when it's time to activate a sensor
doSensor :: C.Cfg -> TS.NRMState -> Double -> IO (TS.NRMState, B.Behavior)
doSensor c s t = B.behavior c s (t & seconds) (B.DoSensor $ t & seconds)

-- | when it's time to run the control loop
doControl :: C.Cfg -> TS.NRMState -> Double -> IO (TS.NRMState, B.Behavior)
doControl c s t = B.behavior c s (t & seconds) (B.DoControl $ t & seconds)

-- | when a child dies
childDied :: C.Cfg -> TS.NRMState -> Double -> Int -> Int -> IO (TS.NRMState, B.Behavior)
childDied c s t pid status =
  B.behavior c s (t & seconds)
    ( B.ChildDied (Process.ProcessID . SPT.CPid $ fromIntegral pid)
      (if status == 0 then ExitSuccess else ExitFailure status)
    )

-- | when the runtime is shutdown
doShutdown :: C.Cfg -> TS.NRMState -> Double -> IO (TS.NRMState, B.Behavior)
doShutdown c s t = B.behavior c s (t & seconds) B.DoShutdown

-- | when a child produces something on its stdout
doStdout :: C.Cfg -> TS.NRMState -> Double -> Text -> Text -> IO (TS.NRMState, B.Behavior)
doStdout = handleTag URep.StdoutOutput

-- | when a child produces something on its stderr
doStderr :: C.Cfg -> TS.NRMState -> Double -> Text -> Text -> IO (TS.NRMState, B.Behavior)
doStderr = handleTag URep.StderrOutput

-- | when a command was properly started.
registerCmdSuccess :: C.Cfg -> TS.NRMState -> Double -> Text -> Int -> IO (TS.NRMState, B.Behavior)
registerCmdSuccess cfg s t cmdIDT pid =
  B.RegisterCmd <$> (CmdID.fromText cmdIDT) ?? B.Launched (Process.ProcessID $ SPT.CPid $ fromIntegral pid) & \case
    Nothing -> return (s, B.Log "couldn't decode cmdID in registerCmdSuccess nrm.so call")
    Just ev -> B.behavior cfg s (t & seconds) ev

-- | when a command failed even starting.
registerCmdFailure :: C.Cfg -> TS.NRMState -> Double -> Text -> IO (TS.NRMState, B.Behavior)
registerCmdFailure cfg s t cmdIDT =
  B.behavior
    cfg
    s
    (t & seconds)
    ( B.RegisterCmd
      (fromMaybe (panic "couldn't decode cmdID") (CmdID.fromText cmdIDT))
      B.NotLaunched
    )

-- Utilities
handleTag :: URep.OutputType -> C.Cfg -> TS.NRMState -> Double -> Text -> Text -> IO (TS.NRMState, B.Behavior)
handleTag tag c s t cmdIDT msg =
  B.behavior c s (t & seconds)
    ( B.DoOutput
      (fromMaybe (panic "couldn't decode cmdID") (CmdID.fromText cmdIDT))
      tag
      msg
    )
