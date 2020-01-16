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
import NRM.Optparse.Client
import qualified NRM.Types.Configuration as Cfg
import NRM.Types.Messaging.UpstreamReq
import Protolude

type Ex = CString -> IO CString

foreign export ccall showStateExport :: Ex

showStateExport = exportIO E.showState

runExport = exportIO run

run = processREq common req
  where
    common = CommonOpts
      { verbose = Normal,
        jsonPrint = False,
        color = False,
        pubPort = pub,
        rpcPort = rpc,
        upstreamBindAddress = addr
      }
    req = ReqRun Run
      { manifest = undefined :: Manifest,
        spec = undefined :: Cmd.CmdSpec,
        runSliceID = undefined :: C.SliceID,
        detachCmd = True
      }
    rpc = Cfg.rpcPort . Cfg.upstreamCfg $ def
    pub = Cfg.pubPort . Cfg.upstreamCfg $ def
    addr = "localhost" :: Text
