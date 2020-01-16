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

import Data.Default
import FFI.TypeUncurry.Msgpack
import Foreign.C
import NRM.Client
import qualified NRM.ExportIO as E
import NRM.Optparse.Client
import NRM.Types.Cmd
import qualified NRM.Types.Configuration as Cfg
import NRM.Types.Manifest
import NRM.Types.Messaging.UpstreamReq
import NRM.Types.Slice
import Protolude

type Ex = CString -> IO CString

foreign export ccall showStateExport :: Ex

showStateExport = exportIO E.showState

runExport = exportIO run

run :: Int -> Text -> Manifest -> CmdSpec -> SliceID -> IO ()
run port addr manifest spec runSliceID = processReq common req
  where
    common = CommonOpts
      { verbose = Normal,
        jsonPrint = False,
        color = False,
        pubPort = port,
        rpcPort = rpc,
        upstreamBindAddress = addr
      }
    req = ReqRun Run
      { detachCmd = True,
        ..
      }
    rpc = Cfg.rpcPort . Cfg.upstreamCfg $ def
