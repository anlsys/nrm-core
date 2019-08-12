{-# LANGUAGE NoImplicitPrelude #-}

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
    isVerbose
  , C.logfile
  , {-, upstreamBindAddress-}
    {-, upstreamRPCPort-}
    {-, upstreamPubPort-}
    upstreamPubAddress
  , upstreamRpcAddress
  , downstreamEventAddress
  , -- * State
    S.initialState
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
import qualified Nrm.Optparse as O (parseArgDaemonCli)
import qualified Nrm.Types.Configuration as C (Cfg (..), DaemonVerbosity (..), DownstreamCfg (..), UpstreamCfg (..), logfile, verbose)
import qualified Nrm.Types.NrmState as S
import Protolude

-- | Parses Daemon CLI arguments
parseDaemon :: [Text] -> IO C.Cfg
parseDaemon = O.parseArgDaemonCli

-- | Queries configuration for verbosity
isVerbose :: C.Cfg -> Bool
isVerbose c =
  C.Verbose == C.verbose c

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

-- | Behave on downstream message
downstreamReceive :: ByteString -> S.NrmState -> IO (S.NrmState, B.Behavior)
downstreamReceive msg = B.behavior (B.Recv B.DownstreamEvent msg)

-- | Behave on upstream message
upstreamReceive :: ByteString -> S.NrmState -> IO (S.NrmState, B.Behavior)
upstreamReceive msg = B.behavior (B.Recv B.UpstreamReq msg)

-- | Behave on sensor trigger
doSensor :: S.NrmState -> IO (S.NrmState, B.Behavior)
doSensor = B.behavior B.DoSensor

-- | Behave on control trigger
doControl :: S.NrmState -> IO (S.NrmState, B.Behavior)
doControl = B.behavior B.DoControl

-- | Behave on children death
doChildren :: S.NrmState -> IO (S.NrmState, B.Behavior)
doChildren = B.behavior B.DoChildren

-- | Behave on shutdown
doShutdown :: S.NrmState -> IO (S.NrmState, B.Behavior)
doShutdown = B.behavior B.DoShutdown
