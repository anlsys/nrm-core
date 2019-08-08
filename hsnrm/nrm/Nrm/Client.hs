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
import qualified Nrm.Types.Messaging.UpstreamReq as UpstreamReq
{-import qualified Nrm.Types.Messaging.UpstreamRep as UpstreamRep-}
{-import qualified Nrm.Types.Messaging.Protocols as P-}
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

client :: Socket z Dealer -> UpstreamReq.Req -> ZMQ z b
client s req = do
  send s [] (toS $ encode req)
  forever $ do
    receive s >>= liftIO . print
    liftIO $ hFlush stdout

{-go :: Protocol -}

{-stream :: Socket z Dealer -> Upstream.Req -> ZMQ z b-}
{-stream s req = do-}
  {-send s [] (toS $ encode req)-}
  {-forever $ do-}
    {-receive s >>= liftIO . print-}
    {-liftIO $ hFlush stdout-}
