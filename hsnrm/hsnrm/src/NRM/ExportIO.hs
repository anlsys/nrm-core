-- |
-- Module      : ExportIO
-- Copyright   : (c) 2019, UChicago Argonne, LLC.
-- License     : BSD3
-- Maintainer  : fre@freux.fr
module NRM.ExportIO
  ( -- * CLI interfaces
    E.parseDaemon,

    -- * Configuration queries
    verbosity,
    showConfiguration,
    logfile,
    activeSensorFrequency,
    upstreamPubAddress,
    upstreamRpcAddress,
    downstreamEventAddress,

    -- * State
    E.initialState,
    showState,

    -- * Event to behavior entry points
    E.downstreamReceive,
    E.upstreamReceive,
    E.doStdout,
    E.doStderr,
    E.doSensor,
    E.doControl,
    E.childDied,
    E.doShutdown,
    E.registerCmdSuccess,
    E.registerCmdFailure,
  )
where

import qualified NRM.Export as E
import qualified NRM.Types.Configuration as C
import qualified NRM.Types.State as TS
import Protolude

verbosity :: C.Cfg -> IO Int
verbosity = return . E.verbosity

logfile :: C.Cfg -> IO Text
logfile = return . E.logfile

activeSensorFrequency :: C.Cfg -> IO Double
activeSensorFrequency = return . E.activeSensorFrequency

upstreamPubAddress :: C.Cfg -> IO Text
upstreamPubAddress = return . E.upstreamPubAddress

upstreamRpcAddress :: C.Cfg -> IO Text
upstreamRpcAddress = return . E.upstreamRpcAddress

downstreamEventAddress :: C.Cfg -> IO Text
downstreamEventAddress = return . E.downstreamEventAddress

showConfiguration :: C.Cfg -> IO Text
showConfiguration = return . E.showConfiguration

showState :: TS.NRMState -> IO Text
showState = return . E.showState
