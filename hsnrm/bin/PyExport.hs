{-# LANGUAGE ScopedTypeVariables #-}
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

import CPD.Core (Problem)
import qualified Data.ByteString as BS
import Data.Default
import Data.Restricted
import FFI.TypeUncurry.Msgpack
import Foreign.C
import LMap.Map as LM
import NRM.Classes.Messaging
import NRM.Client
import NRM.Optparse.Client
import NRM.Optparse.Daemon
import NRM.Types.Cmd
import NRM.Types.Manifest
import NRM.Types.Messaging.UpstreamRep as Rep
import NRM.Types.Messaging.UpstreamReq as Req
import NRM.Types.Slice
import NRM.Types.State
import qualified NRM.Types.UpstreamClient as UC
import Protolude
import qualified System.ZMQ4.Monadic as ZMQ
import Text.Pretty.Simple

type Ex = CString -> IO CString

foreign export ccall showStateExport :: Ex

foreign export ccall showCpdExport :: Ex

foreign export ccall defaultCommonOptsExport :: Ex

foreign export ccall mkSimpleRunExport :: Ex

foreign export ccall runExport :: Ex

foreign export ccall pubAddressExport :: Ex

foreign export ccall rpcAddressExport :: Ex

foreign export ccall finishedExport :: Ex

foreign export ccall cpdExport :: Ex

foreign export ccall stateExport :: Ex

defaultCommonOptsExport :: Ex
defaultCommonOptsExport = exportIO (return def :: IO CommonOpts)

mkSimpleRunExport :: Ex
mkSimpleRunExport = exportIO mkSimpleRun
  where
    mkSimpleRun :: Text -> [Text] -> [(Text, Text)] -> Text -> Text -> IO Run
    mkSimpleRun cmd args env manifest sliceID = do
      m <- processType (Proxy :: Proxy Manifest) Yaml (toS manifest)
      return $ Run
        { manifest = m,
          spec = CmdSpec
            { cmd = Command cmd,
              args = Arguments (Arg <$> args),
              env = Env $ LM.fromList env
            },
          runSliceID = parseSliceID sliceID,
          detachCmd = False
        }

pubAddressExport :: Ex
pubAddressExport = exportIO ((return :: a -> IO a) . pubAddress)

rpcAddressExport :: Ex
rpcAddressExport = exportIO ((return :: a -> IO a) . rpcAddress)

showStateExport :: Ex
showStateExport = exportIO (return . toS . pShow :: NRMState -> IO Text)

showCpdExport :: Ex
showCpdExport = exportIO (return . toS . pShow :: Problem -> IO Text)

runExport :: Ex
runExport = exportIO $ \c runreq -> doReqRep c (ReqRun runreq) $ \case
  (RepStart start) -> start
  (RepStartFailure StartFailure) -> panic "command start failed."
  _ -> panic "command run failed"

stateExport :: Ex
stateExport = exportIO $ \c -> doReqRep c (ReqGetState Req.GetState) $ \case
  (RepGetState st) -> st
  _ -> panic "reply wasn't in protocol"

finishedExport :: Ex
finishedExport = exportIO $ \common -> doReqRep common (ReqSliceList Req.SliceList) $ \case
  (RepList (Rep.SliceList l)) -> l & \case
    [] -> True
    _ -> False
  _ -> panic "command 'slicelist' failed"

cpdExport :: Ex
cpdExport = exportIO $ \c -> doReqRep c (ReqCPD Req.CPD) $ \case
  (RepCPD problem) -> problem
  _ -> panic "reply wasn't in protocol"

doReqRep :: CommonOpts -> Req -> (Rep.Rep -> a) -> IO a
doReqRep common req f = do
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
      Just x -> return (f x)
