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
  ( getDefaultRAPLDirs
  )
where

import FFI.TypeUncurry.Msgpack
import Foreign.C
import qualified Nrm.Node.Sysfs as NrmSysfs (getDefaultRAPLDirs)
{-import qualified Nrm.Node.Sysfs as NrmSysfs (getDefaultRAPLDirs, readRAPLConfigurations)-}
import Protolude

foreign export ccall getDefaultRAPLDirs :: CString -> IO CString

getDefaultRAPLDirs :: CString -> IO CString
getDefaultRAPLDirs = exportIO NrmSysfs.getDefaultRAPLDirs

{-getDefaultRAPLDirs :: CString -> IO CString-}

{-foreign export ccall readRAPLConfigurationsExport :: CString -> IO CString-}

{-readRAPLConfigurationsExport :: CString -> IO CString-}
{-readRAPLConfigurationsExport = exportIO readRAPLConfigurations-}
