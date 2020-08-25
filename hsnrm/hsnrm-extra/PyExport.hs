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

import CPD.Core (ActuatorID (..), Discrete (..), Problem, prettyCPD)
import CPD.Values (Action (..))
import Data.Aeson as A
import qualified Data.ByteString as BS
import Data.Default
import Data.Map as M
import Data.Restricted
import FFI.TypeUncurry.Msgpack
import Foreign.C
import NRM.Classes.Messaging as M
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

foreign export ccall jsonStateExport :: Ex

foreign export ccall showCpdExport :: Ex

foreign export ccall jsonCpdExport :: Ex

foreign export ccall defaultCommonOptsExport :: Ex

foreign export ccall mkSimpleRunExport :: Ex

foreign export ccall runExport :: Ex

foreign export ccall pubAddressExport :: Ex

foreign export ccall rpcAddressExport :: Ex

foreign export ccall finishedExport :: Ex

foreign export ccall cpdExport :: Ex

foreign export ccall actionExport :: Ex

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
              args = Arg <$> args,
              env = Env $ M.fromList env
            },
          runSliceID = parseSliceID sliceID,
          detachCmd = True
        }

pubAddressExport :: Ex
pubAddressExport = exportIO ((return :: a -> IO a) . pubAddress)

rpcAddressExport :: Ex
rpcAddressExport = exportIO ((return :: a -> IO a) . rpcAddress)

showStateExport :: Ex
showStateExport = exportIO (return . toS . pShow :: NRMState -> IO Text)

jsonStateExport :: Ex
jsonStateExport = exportIO (return . toS . A.encode :: NRMState -> IO Text)

showCpdExport :: Ex
showCpdExport = exportIO (return . prettyCPD :: Problem -> IO Text)

jsonCpdExport :: Ex
jsonCpdExport = exportIO (return . toS . A.encode :: Problem -> IO Text)

actionExport :: Ex
actionExport = exportIO actuate
  where
    actuate :: CommonOpts -> [(Text, Double)] -> IO Bool
    actuate c actions =
      doReqRep c (ReqActuate (toAction <$> actions)) . Right $ \case
        (Rep.RepActuate Rep.Actuated) -> True
        (Rep.RepActuate Rep.NotActuated) -> False
        _ -> panic "action protocol failed"
    toAction :: (Text, Double) -> Action
    toAction (textID, doubleAction) = Action (ActuatorID textID) (DiscreteDouble doubleAction)

runExport :: Ex
runExport = exportIO $ \c runreq -> doReqRep c (ReqRun runreq) $ Left ()

stateExport :: Ex
stateExport =
  exportIO $ \c ->
    doReqRep c (ReqGetState Req.GetState) . Right $ \case
      (RepGetState st) -> st
      _ -> protoError

finishedExport :: Ex
finishedExport =
  exportIO $ \common ->
    doReqRep common (ReqSliceList Req.SliceList) . Right $ \case
      (RepList (Rep.SliceList l)) ->
        l & \case
          [] -> True
          _ -> False
      _ -> protoError

cpdExport :: Ex
cpdExport =
  exportIO $ \c ->
    doReqRep c (ReqCPD Req.CPD) . Right $ \case
      (RepCPD problem) -> problem
      _ -> protoError

doReqRep :: CommonOpts -> Req -> Either a (Rep.Rep -> a) -> IO a
doReqRep common req eitherF = do
  uuid <-
    UC.nextUpstreamClientID <&> \case
      Nothing -> panic "couldn't generate next client ID"
      Just c ->
        restrict (toS $ UC.toText c) ::
          Restricted (N1, N254) BS.ByteString
  ZMQ.runZMQ $ do
    s <- ZMQ.socket ZMQ.Dealer
    connectWithOptions uuid common s
    ZMQ.send s [] (toS $ M.encode req)
    eitherF & \case
      Left x' -> return x'
      Right f ->
        ZMQ.receive s <&> decodeT . toS >>= \case
          Nothing -> panic "Couldn't decode reply"
          Just (RepException text) -> panic $ "daemon threw exception: " <> text
          Just m -> return (f m)

protoError :: a
protoError = panic "protocol error"
