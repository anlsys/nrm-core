{-# LANGUAGE ForeignFunctionInterface #-}
{-# LANGUAGE NoImplicitPrelude #-}

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

foreign export ccall cliExport :: CString -> IO CString

cliExport = exportIO E.parseDaemon

foreign export ccall verboseExport :: CString -> IO CString

foreign export ccall logfileExport :: CString -> IO CString

foreign export ccall upstreamRpcAddressExport :: CString -> IO CString

foreign export ccall upstreamPubAddressExport :: CString -> IO CString

foreign export ccall downstreamEventAddressExport :: CString -> IO CString

verboseExport = export E.isVerbose

logfileExport = export E.logfile

upstreamRpcAddressExport = export E.upstreamRpcAddress

upstreamPubAddressExport = export E.upstreamPubAddress

downstreamEventAddressExport = export E.downstreamEventAddress
