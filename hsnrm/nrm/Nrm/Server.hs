{-# LANGUAGE NoImplicitPrelude #-}

{-|
Module      : Nrm.Server
Copyright   : (c) 2019, UChicago Argonne, LLC.
License     : BSD3
Maintainer  : fre@freux.fr
-}
module Nrm.Server
  ( main
  )
where

import Control.Monad
import qualified Data.ByteString as SB
import Data.Restricted
import Nrm.Optparse (parseDaemonCli)
import Nrm.Types.Client
import Protolude
import System.IO (hFlush)
import System.ZMQ4.Monadic as ZMQ

address :: Text
address = "tcp://localhost:3456"

-- | The main nrm client process
main :: IO ()
main = do
  req <- parseDaemonCli
  print req
  uuid <-
    nextClientUUID <&> \case
      Nothing -> panic "couldn't generate next client UUID"
      Just c -> case toRestricted (show c) of
        Nothing -> panic "client UUID can not be casted to Restricted"
        Just r -> r :: Restricted (N1, N254) SB.ByteString
  let hwm = fromMaybe (panic "Can not form proper HWM parameter") (toRestricted (0 :: Integer))
  runZMQ $ do
    s <- socket Router
    ZMQ.setIdentity uuid s
    ZMQ.setSendHighWM hwm s
    ZMQ.setReceiveHighWM hwm s
    connect s (toS address)
    server s

server :: Socket z Router -> ZMQ z b
server s =
  forever $ do
    receive s >>= liftIO . print
    liftIO $ hFlush stdout
