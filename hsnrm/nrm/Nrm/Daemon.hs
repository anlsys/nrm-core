{-# LANGUAGE NoImplicitPrelude #-}

{-|
Module      : Nrm.Daemon
Copyright   : (c) 2019, UChicago Argonne, LLC.
License     : BSD3
Maintainer  : fre@freux.fr
-}
module Nrm.Daemon
  ( main
  , server
  , dummyReply
  , dummy
  )
where

import Control.Monad
import Data.List.NonEmpty
import Data.Restricted
import Nrm.Classes.Messaging
import Nrm.Optparse (parseDaemonCli)
import qualified Nrm.Types.Messaging.Protocols as Protocols
import qualified Nrm.Types.Messaging.UpstreamRep as Rep
import qualified Nrm.Types.Messaging.UpstreamReq as Req
import Protolude
import System.IO (hFlush)
import System.ZMQ4.Monadic as ZMQ

address :: Text
address = "tcp://*:3456"

-- | The main user facing nrm daemon process
main :: IO ()
main = do
  req <- parseDaemonCli
  print req
  runZMQ $ do
    s <- socket Router
    ZMQ.setSendHighWM (restrict (0 :: Int)) s
    ZMQ.setReceiveHighWM (restrict (0 :: Int)) s
    bind s (toS address)
    server s

server :: Socket z Router -> ZMQ z b
server s =
  forever $
    receiveMulti s >>= \case
    [clientUUID, msg] -> do
      putText "Received raw message:"
      liftIO $ hFlush stdout
      case decode $ toS msg of
        Nothing -> putText $ "couldn't decode message: " <> toS msg
        Just req -> do
          liftIO $ print req
          liftIO $ hFlush stdout
          dummyReply req s clientUUID
    _ -> panic "received a message with more than two parts:"

dummyReply :: Req.Req -> Socket z Router -> ByteString -> ZMQ z ()
dummyReply = \case
  (Req.ContainerList x) -> sendOne $ encode (Rep.RepList (dummy Protocols.ContainerList x))
  (Req.Kill x) -> sendOne $ encode (Rep.RepProcessExit (dummy Protocols.Kill x))
  (Req.SetPower x) -> sendOne $ encode (Rep.RepGetPower (dummy Protocols.SetPower x))
  (Req.Run _) -> panic "no run reply implemented in this dummy mode."

dummy :: Protocols.ReqRep req rep -> req -> rep
dummy = \case
  Protocols.ContainerList -> const $ Rep.ContainerList ["foo", "bar"]
  Protocols.SetPower -> const $ Rep.GetPower "266"
  Protocols.Kill -> const $ Rep.ProcessExit "foo" "1"

sendOne :: (Sender t) => ByteString -> Socket z t -> ByteString -> ZMQ z ()
sendOne reply s clientUUID = sendMulti s (fromList [clientUUID, reply])
