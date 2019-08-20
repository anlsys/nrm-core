{-# OPTIONS_GHC -fno-warn-missing-signatures #-}

{-|
Module      : export
Copyright   : (c) 2019, UChicago Argonne, LLC.
License     : BSD3
Maintainer  : fre@freux.fr
-}
module Export
  (
  )
where

import FFI.TypeUncurry.Msgpack
import Foreign.C
import qualified Nrm.Export as E
import Protolude

type Ex = CString -> IO CString

foreign export ccall cliExport :: Ex

foreign export ccall initialStateExport :: Ex

foreign export ccall downstreamReceiveExport :: Ex

foreign export ccall upstreamReceiveExport :: Ex

foreign export ccall doSensorExport :: Ex

foreign export ccall doShutdownExport :: Ex

foreign export ccall doControlExport :: Ex

foreign export ccall doChildrenExport :: Ex

foreign export ccall registerCmdExport :: Ex

cliExport = exportIO E.parseDaemon

initialStateExport = exportIO E.initialState

downstreamReceiveExport = exportIO E.downstreamReceive

upstreamReceiveExport = exportIO E.upstreamReceive

doSensorExport = exportIO E.doSensor

doShutdownExport = exportIO E.doShutdown

doControlExport = exportIO E.doControl

doChildrenExport = exportIO E.doChildren

registerCmdExport = exportIO E.registerCmd

foreign export ccall isVerboseExport :: Ex

foreign export ccall isDebugExport :: Ex

foreign export ccall logfileExport :: Ex

foreign export ccall upstreamRpcAddressExport :: Ex

foreign export ccall upstreamPubAddressExport :: Ex

foreign export ccall downstreamEventAddressExport :: Ex

foreign export ccall showStateExport :: Ex

foreign export ccall showConfigurationExport :: Ex

isVerboseExport = export E.isVerbose

isDebugExport = export E.isDebug

logfileExport = export E.logfile

upstreamRpcAddressExport = export E.upstreamRpcAddress

upstreamPubAddressExport = export E.upstreamPubAddress

downstreamEventAddressExport = export E.downstreamEventAddress

showConfigurationExport = export E.showConfiguration

showStateExport = export E.showState
