{-# OPTIONS_GHC -fno-warn-missing-signatures #-}

-- |
-- Module      : export
-- Copyright   : (c) 2019, UChicago Argonne, LLC.
-- License     : BSD3
-- Maintainer  : fre@freux.fr
module PyExport
  ( Ex,
    runExport,
    simpleRunExport,
    defaultCommonOptsExport,
  )
where

import qualified Data.ByteString as BS
import Data.Default
import Data.Restricted
import FFI.TypeUncurry.Msgpack
import Foreign.C
import LMap.Map as LM
import NRM.Classes.Messaging
import NRM.Client
import qualified NRM.ExportIO as E
import NRM.Optparse.Client
import NRM.Types.Cmd
import NRM.Types.Manifest.Yaml
import NRM.Types.Messaging.UpstreamRep as Rep
import NRM.Types.Messaging.UpstreamReq as Req
import NRM.Types.Slice
import qualified NRM.Types.UpstreamClient as UC
import Protolude
import qualified System.ZMQ4.Monadic as ZMQ

type Ex = CString -> IO CString

foreign export ccall showStateExport :: Ex

foreign export ccall defaultCommonOptsExport :: Ex

foreign export ccall simpleRunExport :: Ex

foreign export ccall runExport :: Ex

foreign export ccall finishedExport :: Ex

showStateExport = exportIO E.showState

defaultCommonOptsExport :: Ex
defaultCommonOptsExport = exportIO defaultCommonOpts

simpleRunExport :: Ex
simpleRunExport = exportIO simpleRun

runExport :: Ex
runExport = exportIO run

finishedExport :: Ex
finishedExport = exportIO finished

defaultCommonOpts :: IO CommonOpts
defaultCommonOpts = return def

simpleRun :: Text -> [Text] -> [(Text, Text)] -> Text -> Text -> IO Run
simpleRun cmd args env manifest sliceID = decodeManifest (toS manifest) & \case
  Left _ -> panic "manifest decoding error."
  Right m -> return $ Run
    { manifest = m,
      spec = CmdSpec
        { cmd = Command cmd,
          args = Arguments (Arg <$> args),
          env = Env $ LM.fromList env
        },
      runSliceID = parseSliceID sliceID,
      detachCmd = True
    }

run :: CommonOpts -> Run -> IO ()
run common runreq = processReq common (ReqRun runreq)

finished :: CommonOpts -> IO Bool
finished common = do
  let req = (ReqSliceList Req.SliceList)
  uuid <-
    UC.nextUpstreamClientID <&> \case
      Nothing -> panic "couldn't generate next client ID"
      Just c ->
        restrict (toS $ UC.toText c) ::
          Restricted (N1, N254) BS.ByteString
  ZMQ.runZMQ $ do
    s <- ZMQ.socket ZMQ.Dealer
    connectWithOptions uuid common s
    ZMQ.send s [] (toS $ encode req)
    ZMQ.receive s <&> decodeT . toS >>= \case
      Nothing -> panic "Couldn't decode reply"
      Just (RepList (Rep.SliceList l)) -> l & \case
        [] -> return True
        _ -> return False
      _ -> panic "reply wasn't in protocol"
