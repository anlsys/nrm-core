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

import qualified Data.ByteString as SB
import Data.Restricted
import Nrm.Classes.Messaging
import Nrm.Optparse
import Nrm.Optparse.Client
import qualified Nrm.Types.Messaging.Protocols as Protocols
import Nrm.Types.Messaging.UpstreamRep
import Nrm.Types.Messaging.UpstreamReq
import Nrm.Types.UpstreamClient
import Protolude hiding (Rep)
import System.IO (hFlush)
import qualified System.ZMQ4.Monadic as ZMQ

address :: Text
address = "tcp://localhost:3456"

-- | The main user facing nrm client process
main :: IO ()
main = do
  (Opts req common) <- parseClientCli
  when (verbose common == Verbose) (print $ encode req)
  uuid <-
    nextUpstreamClientID <&> \case
      Nothing -> panic "couldn't generate next client ID"
      Just c -> (restrict (show c) :: Restricted (N1, N254) SB.ByteString)
  ZMQ.runZMQ $ do
    s <- ZMQ.socket ZMQ.Dealer
    ZMQ.setIdentity uuid s
    ZMQ.setSendHighWM (restrict (0 :: Int)) s
    ZMQ.setReceiveHighWM (restrict (0 :: Int)) s
    ZMQ.connect s $ toS address
    client s req (verbose common)

client
  :: ZMQ.Socket z ZMQ.Dealer
  -> Req
  -> ClientVerbosity
  -> ZMQ.ZMQ z ()
client s req v = do
  ZMQ.send s [] (toS $ encode req)
  dispatchProtocol s v req

dispatchProtocol
  :: ZMQ.Socket z ZMQ.Dealer
  -> ClientVerbosity
  -> Req
  -> ZMQ.ZMQ z ()
dispatchProtocol s v = \case
  (ReqContainerList x) -> reqrep s v Protocols.ContainerList x
  (ReqKill x) -> reqrep s v Protocols.Kill x
  (ReqSetPower x) -> reqrep s v Protocols.SetPower x
  (ReqRun x) -> reqstream s v Protocols.Run x

reqrep
  :: ZMQ.Socket z ZMQ.Dealer
  -> ClientVerbosity
  -> Protocols.ReqRep req rep
  -> req
  -> ZMQ.ZMQ z ()
reqrep s _ = \case
  Protocols.ContainerList ->
    const $ do
      msg <- ZMQ.receive s
      liftIO . print $ ((decode $ toS msg) :: Maybe Rep)
      liftIO $ hFlush stdout
  Protocols.SetPower ->
    const $ do
      msg <- ZMQ.receive s
      liftIO . print $ ((decode $ toS msg) :: Maybe Rep)
      liftIO $ hFlush stdout
  Protocols.Kill ->
    const $ do
      msg <- ZMQ.receive s
      liftIO . print $ ((decode $ toS msg) :: Maybe Rep)
      liftIO $ hFlush stdout

reqstream
  :: ZMQ.Socket z ZMQ.Dealer
  -> ClientVerbosity
  -> Protocols.ReqStream req rep
  -> req
  -> ZMQ.ZMQ z ()
reqstream s _ = \case
  Protocols.Run ->
    const $ forever $ do
      msg <- ZMQ.receive s
      liftIO . print $ ((decode $ toS msg) :: Maybe Rep)
      liftIO $ hFlush stdout
