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
import Nrm.Types.Client
import qualified Nrm.Types.Messaging.Protocols as Protocols
import qualified Nrm.Types.Messaging.UpstreamRep as UpstreamRep
import qualified Nrm.Types.Messaging.UpstreamReq as UpstreamReq
import Protolude
import System.IO (hFlush)
import System.ZMQ4.Monadic as ZMQ

address :: Text
address = "tcp://localhost:3456"

-- | The main user facing nrm client process
main :: IO ()
main = do
  (Opts req common) <- parseClientCli
  when (verbose common == Verbose) (print $ encode req)
  uuid <-
    nextClientUUID <&> \case
      Nothing -> panic "couldn't generate next client UUID"
      Just c -> (restrict (show c) :: Restricted (N1, N254) SB.ByteString)
  runZMQ $ do
    s <- socket Dealer
    ZMQ.setIdentity uuid s
    ZMQ.setSendHighWM (restrict (0 :: Int)) s
    ZMQ.setReceiveHighWM (restrict (0 :: Int)) s
    connect s $ toS address
    client s req (verbose common)

client :: Socket z Dealer -> UpstreamReq.Req -> ClientVerbosity -> ZMQ z ()
client s req v = do
  send s [] (toS $ encode req)
  dispatchProtocol s v req

dispatchProtocol :: Socket z Dealer -> ClientVerbosity -> UpstreamReq.Req -> ZMQ z ()
dispatchProtocol s v = \case
  (UpstreamReq.ContainerList x) -> reqrep s v Protocols.ContainerList x
  (UpstreamReq.Kill x) -> reqrep s v Protocols.Kill x
  (UpstreamReq.SetPower x) -> reqrep s v Protocols.SetPower x
  (UpstreamReq.Run x) -> reqstream s v Protocols.Run x

reqrep :: Socket z Dealer -> ClientVerbosity -> Protocols.ReqRep req rep -> req -> ZMQ z ()
reqrep s _ = \case
  Protocols.ContainerList ->
    const $ do
      msg <- receive s
      liftIO . print $ ((decode $ toS msg) :: Maybe UpstreamRep.Rep)
      liftIO $ hFlush stdout
  Protocols.SetPower ->
    const $ do
      msg <- receive s
      liftIO . print $ ((decode $ toS msg) :: Maybe UpstreamRep.Rep)
      liftIO $ hFlush stdout
  Protocols.Kill ->
    const $ do
      msg <- receive s
      liftIO . print $ ((decode $ toS msg) :: Maybe UpstreamRep.Rep)
      liftIO $ hFlush stdout

reqstream :: Socket z Dealer -> ClientVerbosity -> Protocols.ReqStream req rep -> req -> ZMQ z ()
reqstream s _ = \case
  Protocols.Run ->
    const $ forever $ do
      msg <- receive s
      liftIO . print $ ((decode $ toS msg) :: Maybe UpstreamRep.Rep)
      liftIO $ hFlush stdout
