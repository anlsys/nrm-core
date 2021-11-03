{-# OPTIONS_GHC -fno-warn-missing-signatures #-}

-- |
-- Module      : export
-- Copyright   : (c) 2019, UChicago Argonne, LLC.
-- License     : BSD3
-- Maintainer  : swann@anl.gov
module Export
  ( Ex,
  )
where

import FFI.TypeUncurry.Msgpack
import Foreign.C
import qualified NRM.ExportIO as E
import Protolude

-- | All FFI exported names in this module must have this opaque type
-- , must be followed by "Export", and must not use reserved symbols
-- like "stdout" or "stdin".
type Ex = CString -> IO CString

foreign export ccall cliExport :: Ex

foreign export ccall initialStateExport :: Ex

foreign export ccall downstreamReceiveExport :: Ex

foreign export ccall upstreamReceiveExport :: Ex

foreign export ccall doSensorExport :: Ex

foreign export ccall doShutdownExport :: Ex

foreign export ccall doControlExport :: Ex

foreign export ccall childDiedExport :: Ex

foreign export ccall registerCmdSuccessExport :: Ex

foreign export ccall registerCmdFailureExport :: Ex

foreign export ccall doStdoutExport :: Ex

foreign export ccall doStderrExport :: Ex

foreign export ccall verbosityExport :: Ex

foreign export ccall logfileExport :: Ex

foreign export ccall passiveSensorFrequencyExport :: Ex

foreign export ccall upstreamRpcAddressExport :: Ex

foreign export ccall upstreamPubAddressExport :: Ex

foreign export ccall downstreamEventAddressExport :: Ex

foreign export ccall showStateExport :: Ex

foreign export ccall showConfigurationExport :: Ex

cliExport = exportIO E.parseDaemon

initialStateExport = exportIO E.initialState

downstreamReceiveExport = exportIO E.downstreamReceive

upstreamReceiveExport = exportIO E.upstreamReceive

doSensorExport = exportIO E.doSensor

doShutdownExport = exportIO E.doShutdown

doControlExport = exportIO E.doControl

childDiedExport = exportIO E.childDied

registerCmdSuccessExport = exportIO E.registerCmdSuccess

registerCmdFailureExport = exportIO E.registerCmdFailure

doStdoutExport = exportIO E.doStdout

doStderrExport = exportIO E.doStderr

verbosityExport = exportIO E.verbosity

logfileExport = exportIO E.logfile

passiveSensorFrequencyExport = exportIO E.passiveSensorFrequency

upstreamRpcAddressExport = exportIO E.upstreamRpcAddress

upstreamPubAddressExport = exportIO E.upstreamPubAddress

downstreamEventAddressExport = exportIO E.downstreamEventAddress

showConfigurationExport = exportIO E.showConfiguration

showStateExport = exportIO E.showState
