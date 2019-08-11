{-# LANGUAGE ForeignFunctionInterface #-}
{-# LANGUAGE NoImplicitPrelude #-}

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
import Nrm.Export
import Protolude

-- | daemon CLI arguments
foreign export ccall parseArgDaemonCli_export :: CString -> IO CString

parseArgDaemonCli_export :: CString -> IO CString
parseArgDaemonCli_export = exportIO parseDaemon

-- | verbosity config query
foreign export ccall verbose_export :: CString -> IO CString

verbose_export :: CString -> IO CString
verbose_export = export isVerbose
