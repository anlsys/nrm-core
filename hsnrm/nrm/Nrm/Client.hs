{-|
Module      : Nrm.Client
Copyright   : (c) 2019, UChicago Argonne, LLC.
License     : BSD3
Maintainer  : fre@freux.fr
-}
module Nrm.Client
  ( main
  )
where

import Data.Aeson (decode, encode)
import qualified Data.ByteString as SB
import Data.Restricted
import Nrm.Optparse
import Nrm.Types.Client
import qualified Nrm.Types.Messaging.Protocols as Protocols
import qualified Nrm.Types.Messaging.UpstreamRep as UpstreamRep
import qualified Nrm.Types.Messaging.UpstreamReq as UpstreamReq
import Protolude
import System.IO (hFlush)
import System.ZMQ4.Monadic as ZMQ

address :: Text
address = "tcp://localhost:3456"

-- | The main nrm client process
main :: IO ()
main = do
  req <- parseClientCli
  print (encode req)
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
    client s req

client :: Socket z Dealer -> UpstreamReq.Req -> ZMQ z ()
client s req = do
  send s [] (toS $ encode req)
  dispatchProtocol s req

dispatchProtocol :: Socket z Dealer -> UpstreamReq.Req -> ZMQ z ()
dispatchProtocol s = \case
  (UpstreamReq.ContainerList x) -> reqrep s Protocols.ContainerList x
  (UpstreamReq.Kill x) -> reqrep s Protocols.Kill x
  (UpstreamReq.SetPower x) -> reqrep s Protocols.SetPower x
  (UpstreamReq.Run x) -> reqstream s Protocols.Run x

reqrep :: Socket z Dealer -> Protocols.ReqRep req rep -> req -> ZMQ z ()
reqrep s = \case
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

reqstream :: Socket z Dealer -> Protocols.ReqStream req rep -> req -> ZMQ z ()
reqstream s = \case
  Protocols.Run ->
    const $ forever $ do
      msg <- receive s
      liftIO . print $ ((decode $ toS msg) :: Maybe UpstreamRep.Rep)
      liftIO $ hFlush stdout
