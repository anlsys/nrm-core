{-# LANGUAGE NoImplicitPrelude #-}

{-|
Module      : hnrmd
Description : hnrmd
Copyright   : (c) 2019, UChicago Argonne, LLC.
License     : BSD3
Maintainer  : fre@freux.fr
-}
module Hnrmd
  ( main
  )
where

import FFI.Anything.TypeUncurry.Msgpack
import Foreign.C
import Nrm.Node.Hwloc
import Nrm.Node.Sysfs (getDefaultRAPLDirs)
import Protolude

foreign export ccall getDefaultRAPLDirsExport :: CString -> IO CString

getDefaultRAPLDirsExport = export getDefaultRAPLDirs
