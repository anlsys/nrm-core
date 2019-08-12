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
  )
where

import qualified Nrm.Optparse as O (parseArgDaemonCli)
import qualified Nrm.Types.Configuration as C (Cfg (..), DaemonVerbosity (..), DownstreamCfg (..), UpstreamCfg (..), logfile, verbose)
import Protolude

-- | Parses Daemon CLI arguments
parseDaemon :: [Text] -> IO C.Cfg
parseDaemon = O.parseArgDaemonCli

-- | Queries configuration for verbosity
isVerbose :: C.Cfg -> Bool
isVerbose c =
  C.Verbose == C.verbose c

-- | Query upstream bind address from configuration
{-upstreamBindAddress :: C.Cfg -> Text-}
{-upstreamBindAddress = C.upstreamBindAddress . C.upstreamCfg-}

-- | Query upstream rpc port from configuration
{-upstreamRPCPort :: C.Cfg -> Int-}
{-upstreamRPCPort = C.rpcPort . C.upstreamCfg-}

-- | Query upstream pub port from configuration
{-upstreamPubPort :: C.Cfg -> Int-}
{-upstreamPubPort = C.pubPort . C.upstreamCfg-}

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
