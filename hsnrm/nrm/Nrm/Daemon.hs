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
import Data.Restricted
import Nrm.Optparse (parseDaemonCli)
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
    receive s >>= liftIO . print
    liftIO $ hFlush stdout
