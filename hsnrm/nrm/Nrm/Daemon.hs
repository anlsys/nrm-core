{-# LANGUAGE NoImplicitPrelude #-}

{-|
Module      : Nrm.Daemon
Copyright   : (c) 2019, UChicago Argonne, LLC.
License     : BSD3
Maintainer  : fre@freux.fr
-}
module Nrm.Daemon
  ( main
  )
where

import Control.Monad
import Data.Aeson
import Data.Restricted
import Nrm.Optparse (parseDaemonCli)
import qualified Nrm.Types.Messaging.UpstreamRep as Rep (Rep (..))
import qualified Nrm.Types.Messaging.UpstreamReq as Req (Req (..))
import Protolude
import System.IO (hFlush)
import System.ZMQ4.Monadic as ZMQ

address :: Text
address = "tcp://*:3456"

-- | The main nrm client process
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
  forever $ do
    msg <- receive s
    print msg
    liftIO $ hFlush stdout
    req <-
      case decode $ toS msg of
        Nothing -> panic "Couldn't decode incoming message."
        Just r -> liftIO (print r) >> return r
    liftIO $ print req
    liftIO $ hFlush stdout
    send s [] (toS . encode $ dummyReply req)

dummyReply :: Req.Req -> Rep.Rep
dummyReply Req.List = Rep.List {containers = ["foo", "bar"]}
dummyReply (Req.Run _) = Rep.List {containers = ["foo", "bar"]}
dummyReply (Req.Kill _) = Rep.List {containers = ["foo", "bar"]}
dummyReply (Req.SetPower _) = Rep.List {containers = ["foo", "bar"]}
