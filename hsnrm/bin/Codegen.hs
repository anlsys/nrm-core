{-|
Module      : codegen
Description : codegen
Copyright   : (c) 2019, UChicago Argonne, LLC.
License     : BSD3
Maintainer  : fre@freux.fr
-}
module Codegen
  ( main
  )
where

import Protolude hiding (Rep)
import Nrm.Messaging.Downstream
import Nrm.Messaging.UpstreamPub
import Nrm.Messaging.UpstreamRep
import Nrm.Messaging.UpstreamReq
import Codegen.Schema (generatePretty)
import Codegen.CHeader

main :: IO ()
main = do
  writeFile "../gen/nrm_messaging.h" libnrmHeader
  writeFile "../gen/upstreamPub.json" upstreamPubSchema
  writeFile "../gen/upstreamReq.json" upstreamReqSchema
  writeFile "../gen/upstreamRep.json" upstreamRepSchema
  writeFile "../gen/downstreamEvent.json" downstreamEventSchema

upstreamReqSchema :: Text
upstreamReqSchema = generatePretty (Proxy :: Proxy Req)

upstreamRepSchema :: Text
upstreamRepSchema = generatePretty (Proxy :: Proxy Rep)

upstreamPubSchema :: Text
upstreamPubSchema = generatePretty (Proxy :: Proxy Pub)

downstreamEventSchema :: Text
downstreamEventSchema = generatePretty (Proxy :: Proxy Event)

libnrmHeader :: Text
libnrmHeader = toHeader $ toCHeader (Proxy :: Proxy Event)
