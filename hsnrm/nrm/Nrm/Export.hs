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
  , doStdout
  , doStderr
  , doSensor
  , doControl
  , doChildren
  , doShutdown
  , registerCmd
  )
where

import qualified Nrm.Behavior as B
import qualified Nrm.Classes.Messaging as M
import qualified Nrm.NrmState as S
import qualified Nrm.Optparse as O (parseArgDaemonCli)
{-import qualified Nrm.Types.Container as Ct-}
import qualified Nrm.Types.Configuration as C
  ( Cfg (..)
  , DaemonVerbosity (..)
  , DownstreamCfg (..)
  , UpstreamCfg (..)
  , logfile
  , verbose
  )
import qualified Nrm.Types.NrmState as TS
import qualified Nrm.Types.Process as P
import qualified Nrm.Types.UpstreamClient as UC
import Protolude hiding (stderr, stdout)
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
downstreamReceive :: C.Cfg -> TS.NrmState -> Text -> IO (TS.NrmState, B.Behavior)
downstreamReceive cfg s msg =
  B.behavior cfg s $
    B.DownstreamEvent $
    fromMaybe (panic "couldn't decode downstream rcv")
      (M.decodeT $ toS msg)

-- | Behave on upstream message
upstreamReceive :: C.Cfg -> TS.NrmState -> Text -> Text -> IO (TS.NrmState, B.Behavior)
upstreamReceive cfg s msg clientid =
  B.behavior cfg s $
    B.Req
      ( fromMaybe
        (panic "couldn't parse upstream client ID")
        (UC.fromText clientid)
      )
      ( fromMaybe (panic "couldn't decode downstream rcv")
        (M.decodeT msg)
      )

-- | Behave on sensor trigger
doSensor :: C.Cfg -> TS.NrmState -> IO (TS.NrmState, B.Behavior)
doSensor c s = B.behavior c s B.DoSensor

-- | Behave on control trigger
doControl :: C.Cfg -> TS.NrmState -> IO (TS.NrmState, B.Behavior)
doControl c s = B.behavior c s B.DoControl

-- | Behave on children death
doChildren :: C.Cfg -> TS.NrmState -> IO (TS.NrmState, B.Behavior)
doChildren c s = B.behavior c s B.DoChildren

-- | Behave on shutdown
doShutdown :: C.Cfg -> TS.NrmState -> IO (TS.NrmState, B.Behavior)
doShutdown c s = B.behavior c s B.DoShutdown

-- | Handle stdout
doStdout :: C.Cfg -> TS.NrmState -> Text -> Text -> IO (TS.NrmState, B.Behavior)
doStdout = handleTag B.Stdout

-- | Handle stderr
doStderr :: C.Cfg -> TS.NrmState -> Text -> Text -> IO (TS.NrmState, B.Behavior)
doStderr = handleTag B.Stderr

-- | Register a command as failed or successful.
registerCmd :: C.Cfg -> TS.NrmState -> Text -> Text -> Bool -> IO (TS.NrmState, B.Behavior)
registerCmd cfg s clientid cmdIDT success =
  B.behavior
    cfg
    s
    ( B.RegisterCmd
      ( fromMaybe
        (panic "couldn't parse upstream client ID")
        (UC.fromText clientid)
      )
      (fromMaybe (panic "couldn't decode cmdID") (P.fromText cmdIDT))
      (if success then B.Launched else B.NotLaunched)
    )

-- Utilities
handleTag :: B.OutputType -> C.Cfg -> TS.NrmState -> Text -> Text -> IO (TS.NrmState, B.Behavior)
handleTag tag c s cmdIDT msg =
  B.behavior c s
    ( B.DoOutput
      (fromMaybe (panic "couldn't decode cmdID") (P.fromText cmdIDT))
      tag
      msg
    )
