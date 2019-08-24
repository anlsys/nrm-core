{-|
Module      : Nrm.Client
Copyright   : (c) 2019, UChicago Argonne, LLC.
License     : BSD3
Maintainer  : fre@freux.fr
-}
module Nrm.Client
  ( main
  , client
  , reqrep
  , dispatchProtocol
  )
where

import Data.Aeson.Encode.Pretty as AP (encodePretty)
import qualified Data.ByteString as SB
import Data.Restricted
import Nrm.Classes.Messaging
import Nrm.Optparse
import qualified Nrm.Optparse.Client as C
import qualified Nrm.Types.Container as Ct
import qualified Nrm.Types.Messaging.Protocols as Protocols
import qualified Nrm.Types.Messaging.UpstreamRep as Rep
import qualified Nrm.Types.Messaging.UpstreamReq as Req
import Nrm.Types.Messaging.UpstreamReq
import Nrm.Types.NrmState
import qualified Nrm.Types.Process as P
import Nrm.Types.UpstreamClient
import Protolude hiding (Rep)
import System.IO (hFlush)
import qualified System.Posix.Signals as SPS
  ( Handler (..)
  , installHandler
  , keyboardSignal
  )
import qualified System.ZMQ4.Monadic as ZMQ
import Text.Pretty.Simple

address :: Text
address = "tcp://localhost:3456"

type RestrictedID = Restricted (N1, N254) SB.ByteString

-- | The main user facing nrm client process
main :: IO ()
main = do
  (C.Opts req common) <- parseClientCli
  when (C.verbose common == Verbose) (print $ encode req)
  uuid <-
    nextUpstreamClientID <&> \case
      Nothing -> panic "couldn't generate next client ID"
      Just c -> (restrict (toS $ toText c) :: Restricted (N1, N254) SB.ByteString)
  ZMQ.runZMQ $ do
    s <- ZMQ.socket ZMQ.Dealer
    ZMQ.setIdentity uuid s
    ZMQ.setSendHighWM (restrict (0 :: Int)) s
    ZMQ.setReceiveHighWM (restrict (0 :: Int)) s
    ZMQ.connect s $ toS address
    client s uuid req common

client
  :: ZMQ.Socket z ZMQ.Dealer
  -> RestrictedID
  -> Req
  -> C.CommonOpts
  -> ZMQ.ZMQ z ()
client s uuid req v = do
  ZMQ.send s [] (toS $ encode req)
  dispatchProtocol s uuid v req

dispatchProtocol
  :: ZMQ.Socket z ZMQ.Dealer
  -> RestrictedID
  -> C.CommonOpts
  -> Req
  -> ZMQ.ZMQ z ()
dispatchProtocol s uuid v = \case
  (ReqContainerList x) -> reqrep s v Protocols.ContainerList x
  (ReqKillContainer x) -> reqrep s v Protocols.KillContainer x
  (ReqKillCmd x) -> reqrep s v Protocols.KillCmd x
  (ReqSetPower x) -> reqrep s v Protocols.SetPower x
  (ReqGetConfig x) -> reqrep s v Protocols.GetConfig x
  (ReqGetState x) -> reqrep s v Protocols.GetState x
  (ReqRun x) -> if detachCmd x then putText "Client detached." else reqstream s uuid v Protocols.Run x

reqrep
  :: ZMQ.Socket z ZMQ.Dealer
  -> C.CommonOpts
  -> Protocols.ReqRep req rep
  -> req
  -> ZMQ.ZMQ z ()
reqrep s opts = \case
  Protocols.ContainerList ->
    const $ do
      ZMQ.receive s <&> decode . toS >>= \case
        Nothing -> putText "Couldn't decode reply"
        Just (Rep.RepList (Rep.ContainerList l)) -> case length l of
          0 -> putText "No containers currently running."
          x -> do
            putText (show x <> " container(s) currently running.")
            putText $ showContainerList l
        _ -> putText "reply wasn't in protocol"
      liftIO $ hFlush stdout
  Protocols.GetState ->
    const $ do
      msg <- ZMQ.receive s <&> toS
      case decode msg of
        Nothing -> putText "Couldn't decode reply"
        Just (Rep.RepGetState (Rep.GetState st)) ->
          if C.jsonPrint opts
          then putText $ toS (AP.encodePretty st)
          else
            do
              putText "Current daemon state:"
              pPrint st
        _ -> putText "reply wasn't in protocol"
      liftIO $ hFlush stdout
  Protocols.GetConfig ->
    const $ do
      ZMQ.receive s <&> decode . toS >>= \case
        Nothing -> putText "Couldn't decode reply"
        Just (Rep.RepGetConfig (Rep.GetConfig cfg)) ->
          if C.jsonPrint opts
          then putText $ toS (AP.encodePretty cfg)
          else
            do
              putText "Daemon configuration:"
              pPrint cfg
        _ -> putText "reply wasn't in protocol"
      liftIO $ hFlush stdout
  Protocols.SetPower ->
    const $ do
      msg <- ZMQ.receive s
      liftIO . print $ ((decode $ toS msg) :: Maybe Rep.Rep)
      liftIO $ hFlush stdout
  Protocols.KillCmd ->
    const $ do
      msg <- ZMQ.receive s
      liftIO . print $ ((decode $ toS msg) :: Maybe Rep.Rep)
      liftIO $ hFlush stdout
  Protocols.KillContainer ->
    const $ do
      msg <- ZMQ.receive s
      liftIO . print $ ((decode $ toS msg) :: Maybe Rep.Rep)
      liftIO $ hFlush stdout

reqstream
  :: ZMQ.Socket z ZMQ.Dealer
  -> RestrictedID
  -> C.CommonOpts
  -> Protocols.ReqStream req rep
  -> req
  -> ZMQ.ZMQ z ()
reqstream s uuid _ = \case
  Protocols.Run -> \(Req.Run {..}) -> do
    msg <- ZMQ.receive s
    case ((decode $ toS msg) :: Maybe Rep.Rep) of
      Nothing -> putText "error: received malformed message(1)."
      Just (Rep.RepStart (Rep.Start containerID cmdID)) -> do
        kbInstallHandler (liftIO $ kill uuid cmdID)
        go
      _ -> putText "error: received wrong type of message(1)."
    where
      go = do
        msg <- ZMQ.receive s
        case ((decode $ toS msg) :: Maybe Rep.Rep) of
          Nothing -> putText "error: received malformed message."
          Just (Rep.RepStdout x) -> do
            liftIO . putText . Rep.stdoutPayload $ x
            go
          Just (Rep.RepStderr x) -> do
            liftIO . putErrText . Rep.stderrPayload $ x
            go
          Just (Rep.RepEndStream Rep.EndStream) -> return ()
          _ -> putText "error: received wrong type of message."
      {-kbInstallHandler :: (MonadIO m) => IO () -> m SPS.Handler-}
      kbInstallHandler :: IO () -> ZMQ.ZMQ z SPS.Handler
      kbInstallHandler h =
        liftIO $ SPS.installHandler SPS.keyboardSignal (SPS.Catch h) Nothing

kill :: RestrictedID -> P.CmdID -> IO ()
kill uuid cmdID =
  ZMQ.runZMQ $ do
    s <- ZMQ.socket ZMQ.Dealer
    ZMQ.setIdentity uuid s
    ZMQ.setSendHighWM (restrict (0 :: Int)) s
    ZMQ.setReceiveHighWM (restrict (0 :: Int)) s
    ZMQ.connect s $ toS address
    ZMQ.send s [] (toS $ encode $ Req.ReqKillContainer (Req.KillCmd cmdID))
