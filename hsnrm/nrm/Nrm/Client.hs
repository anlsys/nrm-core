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

import Control.Monad
import Data.Aeson (encode)
import qualified Data.ByteString as SB
import Data.Restricted
import Nrm.Optparse
import Nrm.Types.Client
import qualified Nrm.Types.Messaging.UpstreamReq as Upstream
import Protolude
import System.IO (hFlush)
import System.ZMQ4.Monadic as ZMQ

address :: Text
address = "tcp://localhost:3456"

-- | The main nrm client process
main :: IO ()
main = do
  req <- parseClientCli
  print req
  uuid <-
    nextClientUUID <&> \case
      Nothing -> panic "couldn't generate next client UUID"
      Just c -> case toRestricted (show c) of
        Nothing -> panic "client UUID can not be casted to Restricted"
        Just r -> r :: Restricted (N1, N254) SB.ByteString
  let hwm = fromMaybe (panic "Can not form proper HWM parameter") (toRestricted (0 :: Integer))
  runZMQ $ do
    s <- socket Dealer
    ZMQ.setIdentity uuid s
    ZMQ.setSendHighWM hwm s
    ZMQ.setReceiveHighWM hwm s
    connect s (toS address)
    client s req

client :: Socket z Dealer -> Upstream.Req -> ZMQ z b
client s req = do
  send s [] (toS $ encode req)
  forever $ do
    receive s >>= liftIO . print
    liftIO $ hFlush stdout
