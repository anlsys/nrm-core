{-# LANGUAGE QuasiQuotes #-}
{-# LANGUAGE ScopedTypeVariables #-}

-- |
-- Module      : NRM.Codegen
-- Copyright   : (c) 2019, UChicago Argonne, LLC.
-- License     : BSD3
-- Maintainer  : fre@freux.fr
module NRM.Codegen
  ( main
  , typeToFile
  , upstreamPubSchema
  , upstreamReqSchema
  , upstreamRepSchema
  , downstreamEventSchema
  , manifestSchema
  , configurationSchema
  , libnrmHeader
  , licenseC
  , licenseDhall
  , licenseYaml
  )
where

import Codegen.CHeader
import Codegen.Dhall
import Codegen.Schema (generatePretty)
import Codegen.Schema as CS
import Data.Aeson.Encode.Pretty as AP (encodePretty)
import Data.Default
import Data.JSON.Schema as S
import qualified Data.Map as M
import Data.Yaml as Y
import Dhall
import qualified Dhall.Core as Dhall
import Dhall.Import as Dhall
import Dhall.JSON as DJ
import qualified Dhall.Lint as Lint
import qualified Dhall.Parser
import qualified Dhall.TypeCheck as Dhall
import NRM.Messaging
import qualified NRM.Types.Configuration as C
import qualified NRM.Types.Manifest as MI
import NRM.Types.Messaging.DownstreamEvent
import qualified NRM.Types.Messaging.DownstreamEvent as Down (Event (..))
import NRM.Types.Messaging.UpstreamPub
import NRM.Types.Messaging.UpstreamRep
import NRM.Types.Messaging.UpstreamReq
import NeatInterpolation
import Protolude hiding (Rep)
import System.Directory

-- | The main code generation binary.
main :: IO ()
main = do
  (toS -> prefix) : _ <- getArgs
  putText "Codegen: LibNRM C headers."
  putText $ "  Writing libnrm header to " <> prefix <> "/nrm_messaging.h"
  writeFile (toS $ prefix <> "/nrm_messaging.h") $ toS (licenseC <> "\n\n" <> libnrmVars <> "\n\n" <> libnrmHeader)
  putText "Codegen: JSON schemas"
  verboseWriteSchema prefix "upstreamPub" upstreamPubSchema
  verboseWriteSchema prefix "upstreamRep" upstreamRepSchema
  verboseWriteSchema prefix "upstreamReq" upstreamReqSchema
  verboseWriteSchema prefix "downstreamEvent" downstreamEventSchema
  verboseWriteSchema prefix "manifestSchema" manifestSchema
  generateResources prefix
  where
    verboseWriteSchema :: Text -> Text -> Text -> IO ()
    verboseWriteSchema prefix desc sch = do
      putText $ toS ("  Writing schema for " <> toS desc <> " to " <> fp)
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
  [text|
    /*******************************************************************************
     * Copyright 2019 UChicago Argonne, LLC.
     * (c.f. AUTHORS, LICENSE)
     *
     * SPDX-License-Identifier: BSD-3-Clause
    *******************************************************************************
     *
     *    this file is generated, modifications will be erased.
    */

  |]

-- | A license for Yaml files
licenseYaml :: Text
licenseYaml =
  [text|
    # ******************************************************************************
    #  Copyright 2019 UChicago Argonne, LLC.
    #  (c.f. AUTHORS, LICENSE)
    #
    #  SPDX-License-Identifier: BSD-3-Clause
    # ******************************************************************************
    #
    #     this file is generated, modifications will be erased.
    #

  |]

-- | A license for Dhall files
licenseDhall :: Text
licenseDhall =
  [text|
    -- ******************************************************************************
    --  Copyright 2019 UChicago Argonne, LLC.
    --  (c.f. AUTHORS, LICENSE)
    --
    --  SPDX-License-Identifier: BSD-3-Clause
    -- ******************************************************************************
    --
    --     this file is generated, modifications will be erased.
    --

  |]

data KnownType
  = Cfg
  | Manifest
  deriving (Bounded, Enum, Eq, Ord, Read, Show)

dhallType :: KnownType -> Dhall.Expr Dhall.Parser.Src Dhall.Import
dhallType =
  fmap Dhall.absurd <$> \case
    Cfg -> Dhall.expected (Dhall.auto :: Dhall.Type C.Cfg)
    Manifest -> Dhall.expected (Dhall.auto :: Dhall.Type MI.Manifest)

sandwich :: Semigroup a => a -> a -> a -> a
sandwich a b x = a <> x <> b

typeFile :: KnownType -> FilePath
typeFile = sandwich "types/" ".dhall" . show

getDefault :: KnownType -> Dhall.Expr Dhall.Parser.Src b
getDefault x =
  Dhall.absurd <$> case x of
    Cfg -> embed (injectWith defaultInterpretOptions) (def :: C.Cfg)
    Manifest -> embed (injectWith defaultInterpretOptions) (def :: MI.Manifest)

generateResources :: Text -> IO ()
generateResources prefix = do
  putText "Codegen: Dhall types."
  --typeToFile (Proxy :: Proxy [CPD.Values.Measurement]) $ prefix <> "/types/CPDMeasurements.dhall"
  --typeToFile (Proxy :: Proxy [CPD.Values.Action]) $ prefix <> "/types/CPDActions.dhall"
  --typeToFile (Proxy :: Proxy CPD.Core.Problem) $ toS prefix <> "/types/CPDProblem.dhall"
  for_ ([minBound .. maxBound] :: [KnownType]) $ \t -> do
    let dest = toS prefix <> typeFile t
    putText $ "  Writing type for " <> show t <> " to " <> toS dest
    createDirectoryIfMissing True (takeDirectory dest)
    writeOutput licenseDhall dest (dhallType t)
  putText "Codegen: defaults."
  for_ ([minBound .. maxBound] :: [KnownType]) $ \defaultType ->
    Dhall.load (Lint.lint (getDefault defaultType)) >>=
      exprToDir "defaults/" (show defaultType)
  putText "Codegen: example manifests."
  for_ (M.toList MI.examples) $ \(defName, defValue) ->
    Dhall.load (Lint.lint $ Dhall.absurd <$> embed (injectWith defaultInterpretOptions) defValue) >>=
      exprToDir "example-manifests/" defName
  putText "Codegen: example configurations."
  for_ (M.toList C.examples) $ \(defName, defValue) ->
    Dhall.load (Lint.lint $ Dhall.absurd <$> embed (injectWith defaultInterpretOptions) defValue) >>=
      exprToDir "example-configurations/" defName
  where
    exprToDir dir defName expr = do
      let (dest, destJ, destY) = mkPaths dir defName
      DJ.dhallToJSON expr & \case
        Left e -> die $ "horrible internal dhall error: " <> show e
        Right jsonValue -> do
          putText $ "  Writing default for " <> defName <> " to " <> dest <> "."
          createDirectoryIfMissing True (takeDirectory $ toS dest)
          writeOutput
            licenseDhall
            (toS dest)
            expr
          writeFile (toS destJ) $ toS (AP.encodePretty jsonValue)
          writeFile (toS destY) $ licenseYaml <> toS (Y.encode jsonValue)
    resourcePath dir defName x = toS prefix <> dir <> defName <> x
    mkPaths dir defName =
      ( resourcePath dir defName ".dhall"
      , resourcePath dir defName ".json"
      , resourcePath dir defName ".yaml"
      )

typeToFile :: (Interpret x) => Proxy x -> Text -> IO ()
typeToFile (Proxy :: Proxy x) fp = do
  let destCPD = fp
  putText $ "  Writing types for CPD format. " <> " to " <> toS destCPD
  createDirectoryIfMissing True (takeDirectory $ toS destCPD)
  writeOutput
    licenseDhall
    (toS destCPD)
    (Dhall.expected (Dhall.auto :: Dhall.Type x))
