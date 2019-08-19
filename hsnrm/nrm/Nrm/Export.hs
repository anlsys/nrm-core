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
  , upstreamPubAddress
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
import qualified Nrm.Classes.Messaging as M
import qualified Nrm.NrmState as S
import qualified Nrm.Optparse as O (parseArgDaemonCli)
import qualified Nrm.Types.Configuration as C
  ( Cfg (..)
  , DaemonVerbosity (..)
  , DownstreamCfg (..)
  , UpstreamCfg (..)
  , logfile
  , verbose
  )
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
downstreamReceive :: TS.NrmState -> Text -> IO (TS.NrmState, B.Behavior)
downstreamReceive s msg =
  B.behavior s $
    B.Event $
    fromMaybe (panic "couldn't decode downstream rcv")
      (M.decodeT $ toS msg)

-- | Behave on upstream message
upstreamReceive :: TS.NrmState -> Text -> Text -> IO (TS.NrmState, B.Behavior)
upstreamReceive s msg clientid =
  B.behavior s $
    B.Req
      ( fromMaybe
        (panic "couldn't parse upstream client ID")
        (readMaybe $ toS clientid)
      )
      ( fromMaybe (panic "couldn't decode downstream rcv")
        (M.decodeT msg)
      )

-- | Behave on sensor trigger
doSensor :: TS.NrmState -> IO (TS.NrmState, B.Behavior)
doSensor s = B.behavior s B.DoSensor

-- | Behave on control trigger
doControl :: TS.NrmState -> IO (TS.NrmState, B.Behavior)
doControl s = B.behavior s B.DoControl

-- | Behave on children death
doChildren :: TS.NrmState -> IO (TS.NrmState, B.Behavior)
doChildren s = B.behavior s B.DoChildren

-- | Behave on shutdown
doShutdown :: TS.NrmState -> IO (TS.NrmState, B.Behavior)
doShutdown s = B.behavior s B.DoShutdown
