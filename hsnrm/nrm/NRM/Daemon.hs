{-|
Module      : NRM.Daemon
Copyright   : (c) 2019, UChicago Argonne, LLC.
License     : BSD3
Maintainer  : fre@freux.fr
-}
module NRM.Daemon
  ( main
  , server
  , dummyReply
  , dummy
  )
where

import Control.Monad
import Data.List.NonEmpty
import Data.Restricted
import NRM.Classes.Messaging
import NRM.Optparse (parseDaemonCli)
import qualified NRM.Types.Messaging.Protocols as Protocols
import qualified NRM.Types.Messaging.UpstreamRep as Rep
import qualified NRM.Types.Messaging.UpstreamReq as Req
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

server :: Socket z Router -> ZMQ z v
server s =
  forever $
    receiveMulti s >>= \case
    [clientID, msg] -> do
      putText "Received raw message:"
      liftIO $ hFlush stdout
      case decode $ toS msg of
        Nothing -> putText $ "couldn't decode message: " <> toS msg
        Just req -> do
          liftIO $ print req
          liftIO $ hFlush stdout
          dummyReply req s clientID
    _ -> panic "received a message with more than two parts:"

dummyReply :: Req.Req -> Socket z Router -> ByteString -> ZMQ z ()
dummyReply = \case
  (Req.ReqSliceList x) -> sendOne $ encode (Rep.RepList (dummy Protocols.SliceList x))
  (Req.ReqGetState x) -> sendOne $ encode (Rep.RepGetState (dummy Protocols.GetState x))
  (Req.ReqGetConfig x) -> sendOne $ encode (Rep.RepGetConfig (dummy Protocols.GetConfig x))
  (Req.ReqKillSlice x) -> sendOne $ encode (Rep.RepSliceKilled (dummy Protocols.KillSlice x))
  (Req.ReqKillCmd x) -> sendOne $ encode (Rep.RepCmdKilled (dummy Protocols.KillCmd x))
  (Req.ReqSetPower x) -> sendOne $ encode (Rep.RepGetPower (dummy Protocols.SetPower x))
  (Req.ReqRun _) -> panic "no run reply implemented in this dummy mode."

dummy :: Protocols.ReqRep req rep -> req -> rep
dummy = panic "not implemented"

sendOne :: (Sender t) => ByteString -> Socket z t -> ByteString -> ZMQ z ()
sendOne reply s clientID = sendMulti s (fromList [clientID, reply])
