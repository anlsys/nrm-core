{-# LANGUAGE ScopedTypeVariables #-}

-- |
-- Module      : NRM.Codegen
-- Copyright   : (c) 2019, UChicago Argonne, LLC.
-- License     : BSD3
-- Maintainer  : fre@freux.fr
module NRM.Codegen
  ( main,
    upstreamPubSchema,
    upstreamReqSchema,
    upstreamRepSchema,
    downstreamEventSchema,
    manifestSchema,
    configurationSchema,
    libnrmHeader,
    licenseC,
  )
where

import Codegen.CHeader
import Codegen.Schema (generatePretty)
import Codegen.Schema as CS
import Data.Aeson.Encode.Pretty as AP (encodePretty)
import Data.Default
import Data.JSON.Schema as S
import Dhall
import NRM.Messaging
import qualified NRM.Types.Configuration as C
import qualified NRM.Types.Manifest as MI
import NRM.Types.Messaging.DownstreamEvent
import qualified NRM.Types.Messaging.DownstreamEvent as Down (Event (..))
import NRM.Types.Messaging.UpstreamPub
import NRM.Types.Messaging.UpstreamRep
import NRM.Types.Messaging.UpstreamReq
import Protolude hiding (Rep)

-- | The main code generation binary.
main :: IO ()
main = do
  (toS -> prefix) : _ <- getArgs
  putText "Codegen: LibNRM C headers."
  putText $ "  Writing libnrm header to " <> prefix <> "/nrm_messaging.h"
  writeFile (toS $ prefix <> "/nrm_messaging.h") $ toS (licenseC <> "\n\n" <> libnrmVars <> "\n\n" <> libnrmHeader)
  putText "Codegen: JSON schemas"
  verboseWrite (prefix <> "/schemas") "upstream-pub" upstreamPubSchema
  verboseWrite (prefix <> "/schemas") "upstream-rep" upstreamRepSchema
  verboseWrite (prefix <> "/schemas") "upstream-req" upstreamReqSchema
  verboseWrite (prefix <> "/schemas") "downstream" downstreamEventSchema
  verboseWrite (prefix <> "/schemas") "manifest" manifestSchema
  verboseWrite (prefix <> "/schemas") "nrmd" configurationSchema
  verboseWrite (prefix <> "/defaults") "nrmd" (toS $ AP.encodePretty (def :: C.Cfg))
  verboseWrite (prefix <> "/defaults") "manifest" (toS $ AP.encodePretty (def :: MI.Manifest))
  where
    verboseWrite :: Text -> Text -> Text -> IO ()
    verboseWrite prefix desc sch = do
      putText $ toS ("  Writing file  " <> fp)
      writeFile (toS fp) sch
      where
        fp = prefix <> "/" <> desc <> ".json"

-- | The upstream Request schema.
upstreamReqSchema :: Text
upstreamReqSchema = generatePretty (Proxy :: Proxy Req)

-- | The upstream Reply schema.
upstreamRepSchema :: Text
upstreamRepSchema = generatePretty (Proxy :: Proxy Rep)

-- | The upstream Pub schema.
upstreamPubSchema :: Text
upstreamPubSchema = generatePretty (Proxy :: Proxy Pub)

-- | The downstream Event schema.
downstreamEventSchema :: Text
downstreamEventSchema = generatePretty (Proxy :: Proxy Event)

-- | The manifest schema.
manifestSchema :: Text
manifestSchema = toS . AP.encodePretty . CS.toAeson $ S.schema (Proxy :: Proxy MI.Manifest)

-- | The configuration schema.
configurationSchema :: Text
configurationSchema = toS . AP.encodePretty . CS.toAeson $ S.schema (Proxy :: Proxy C.Cfg)

-- | The libnrm C header.
libnrmHeader :: Text
libnrmHeader = toS $ toCHeader (Proxy :: Proxy Down.Event)

-- | A license for C headers.
licenseC :: Text
licenseC =
  "/*******************************************************************************" <> "\n"
    <> " * Copyright 2019 UChicago Argonne, LLC."
    <> "\n"
    <> " * (c.f. AUTHORS, LICENSE)"
    <> "\n"
    <> " *"
    <> "\n"
    <> " * SPDX-License-Identifier: BSD-3-Clause"
    <> "\n"
    <> "*******************************************************************************"
    <> "\n"
    <> " *"
    <> "\n"
    <> " *    this file is generated, modifications will be erased."
    <> "\n"
    <> "*/"
    <> "\n"
