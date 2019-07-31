{-# LANGUAGE ForeignFunctionInterface #-}
{-# LANGUAGE NoImplicitPrelude #-}

{-|
Module      : export
Description : export
Copyright   : (c) 2019, UChicago Argonne, LLC.
License     : BSD3
Maintainer  : fre@freux.fr
-}
module Export
  ( getDefaultRAPLDirsExport
  )
where

import FFI.Anything.TypeUncurry.Msgpack
import Foreign.C
import Nrm.Node.Sysfs (getDefaultRAPLDirs)
import Protolude

foreign export ccall getDefaultRAPLDirsExport :: CString -> IO CString

getDefaultRAPLDirsExport :: CString -> IO CString
getDefaultRAPLDirsExport = exportIO getDefaultRAPLDirs
