{-# OPTIONS_GHC -fno-warn-missing-signatures #-}

-- |
-- Module      : main
-- Copyright   : (c) 2019, UChicago Argonne, LLC.
-- License     : BSD3
-- Maintainer  : fre@freux.fr
module Shared
  (
  )
where

import FFI.TypeUncurry.Msgpack
import Foreign.C
import Protolude

-- | All FFI exported names in this module must have this opaque type
-- , must be followed by "Export", and must not use reserved symbols
-- like "stdout" or "stdin".
type Ex = CString -> IO CString

foreign export ccall fExport :: Ex
fExport = exportIO doubler

doubler :: Int -> IO Int
doubler i = return (i + i)
