{-|
Module      : export
Copyright   : (c) 2019, UChicago Argonne, LLC.
License     : BSD3
Maintainer  : fre@freux.fr
-}
module Nrm.Export
  ( -- * CLI interfaces
    parseDaemon
  , -- * Configuration queries
    isDebug
  , isVerbose
  , showConfiguration
  , C.logfile
  , {-, upstreamBindAddress-}
    {-, upstreamRPCPort-}
    {-, upstreamPubPort-}
    upstreamPubAddress
  , upstreamRpcAddress
  , downstreamEventAddress
  , -- * State
    S.initialState
  , showState
  , -- * Behavior
    downstreamReceive
  , upstreamReceive
  , doSensor
  , doControl
  , doChildren
  , doShutdown
  )
where

import qualified Nrm.Behavior as B
import qualified Nrm.NrmState as S
import qualified Nrm.Optparse as O (parseArgDaemonCli)
import qualified Nrm.Types.Configuration as C (Cfg (..), DaemonVerbosity (..), DownstreamCfg (..), UpstreamCfg (..), logfile, verbose)
import qualified Nrm.Types.NrmState as TS
import Protolude
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

-- | Query full upstream pub zmq address from configuration
upstreamRpcAddress :: C.Cfg -> Text
upstreamRpcAddress = buildAddress C.rpcPort

-- | Query full upstream pub zmq address from configuration
downstreamEventAddress :: C.Cfg -> Text
downstreamEventAddress = C.downstreamBindAddress . C.downstreamCfg

buildAddress :: Show a => (C.UpstreamCfg -> a) -> C.Cfg -> Text
buildAddress accessor c = "tcp://" <> C.upstreamBindAddress u <> ":" <> show (accessor u)
  where
    u = C.upstreamCfg c

-- | Show the state in text format
showState :: TS.NrmState -> Text
showState = toS . pShow

-- | Behave on downstream message
downstreamReceive :: TS.NrmState -> ByteString -> IO (TS.NrmState, B.Behavior)
downstreamReceive s msg = B.behavior (B.Recv B.DownstreamEvent msg) s

-- | Behave on upstream message
upstreamReceive :: TS.NrmState -> ByteString -> IO (TS.NrmState, B.Behavior)
upstreamReceive s msg = B.behavior (B.Recv B.UpstreamReq msg) s

-- | Behave on sensor trigger
doSensor :: TS.NrmState -> IO (TS.NrmState, B.Behavior)
doSensor = B.behavior B.DoSensor

-- | Behave on control trigger
doControl :: TS.NrmState -> IO (TS.NrmState, B.Behavior)
doControl = B.behavior B.DoControl

-- | Behave on children death
doChildren :: TS.NrmState -> IO (TS.NrmState, B.Behavior)
doChildren = B.behavior B.DoChildren

-- | Behave on shutdown
doShutdown :: TS.NrmState -> IO (TS.NrmState, B.Behavior)
doShutdown = B.behavior B.DoShutdown
