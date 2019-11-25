{-# OPTIONS_GHC -fno-warn-missing-signatures #-}

-- |
-- Module      : export
-- Copyright   : (c) 2019, UChicago Argonne, LLC.
-- License     : BSD3
-- Maintainer  : fre@freux.fr
module PyExport
  ( Ex,
  )
where

import FFI.TypeUncurry.Msgpack
import Foreign.C
import qualified NRM.ExportIO as E
import Protolude

type Ex = CString -> IO CString

foreign export ccall showStateExport :: Ex

showStateExport = exportIO E.showState
