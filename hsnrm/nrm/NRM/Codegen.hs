{-# LANGUAGE ScopedTypeVariables #-}

{-|
Module      : NRM.Codegen
Copyright   : (c) 2019, UChicago Argonne, LLC.
License     : BSD3
Maintainer  : fre@freux.fr
-}
module NRM.Codegen
  ( main
  , upstreamPubSchema
  , upstreamReqSchema
  , upstreamRepSchema
  , downstreamEventSchema
  , libnrmHeader
  , licenseC
  , licenseDhall
  , licenseYaml
  )
where

import qualified CPD.Core
import qualified CPD.Values
import Codegen.CHeader
import Codegen.Dhall
import Codegen.Schema (generatePretty)
import Data.Default
import qualified Data.Map as DM
import Dhall
import qualified Dhall.Core as Dhall
import qualified Dhall.Lint as Lint
import qualified Dhall.Parser
import qualified Dhall.TypeCheck as Dhall
import qualified NRM.Classes.Examples as Examples
import qualified NRM.Manifest.Examples ()
import qualified NRM.Types.Configuration as C
import qualified NRM.Types.Configuration.Yaml as CI (encodeDCfg)
import qualified NRM.Types.Manifest as MI
import qualified NRM.Types.Manifest.Yaml as MI (encodeManifest)
import NRM.Types.Messaging.DownstreamEvent
import qualified NRM.Types.Messaging.DownstreamEvent.JSON as Down (Event (..))
import NRM.Types.Messaging.UpstreamPub
import NRM.Types.Messaging.UpstreamRep
import NRM.Types.Messaging.UpstreamReq
import NeatInterpolation
import Protolude hiding (Rep)
import System.Directory
import Prelude (String)

-- | The main code generation binary.
main :: IO ()
main = do
  (toS -> prefix) : _ <- getArgs
  putText "Codegen: LibNRM C headers."
  putText $ "  Writing libnrm header to " <> prefix <> "/nrm_messaging.h"
  writeFile (toS $ prefix <> "/nrm_messaging.h") $ toS (licenseC <> libnrmHeader)
  putText "Codegen: JSON schemas"
  --verboseWriteSchema prefix "upstreamPub" upstreamPubSchema
  --verboseWriteSchema prefix "upstreamRep" upstreamRepSchema
  --verboseWriteSchema prefix "upstreamReq" upstreamReqSchema
  verboseWriteSchema prefix "downstreamEvent" downstreamEventSchema
  generateDefaultConfigurations prefix
  where
    verboseWriteSchema :: Text -> Text -> Text -> IO ()
    verboseWriteSchema prefix desc sch = do
      putText $ toS ("  Writing schema for " <> toS desc <> " to " <> fp)
      writeFile (toS fp) sch
      where
        fp = prefix <>"/"<> desc <> ".json"

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

-- | The libnrm C header.
libnrmHeader :: Text
libnrmHeader = toHeader $ toCHeader (Proxy :: Proxy Down.Event)

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

dhallType :: KnownType -> Dhall.Expr Dhall.Parser.Src a
dhallType =
  fmap Dhall.absurd <$> \case
    Cfg -> Dhall.expected (Dhall.auto :: Dhall.Type C.Cfg)
    Manifest -> Dhall.expected (Dhall.auto :: Dhall.Type MI.Manifest)

yamlType :: KnownType -> ByteString
yamlType Cfg = CI.encodeDCfg (def :: C.Cfg)
yamlType Manifest = MI.encodeManifest (def :: MI.Manifest)

sandwich :: Semigroup a => a -> a -> a -> a
sandwich a b x = a <> x <> b

yamlFile :: KnownType -> FilePath
yamlFile = sandwich "yaml/" ".yaml" . show

defaultFile :: KnownType -> FilePath
defaultFile = sandwich "defaults/" ".dhall" . show

typeFile :: KnownType -> FilePath
typeFile = sandwich "types/" ".dhall" . show

getDefault :: KnownType -> Dhall.Expr Dhall.Parser.Src b
getDefault x =
  Dhall.absurd <$> case x of
    Cfg -> embed (injectWith defaultInterpretOptions) (def :: C.Cfg)
    Manifest -> embed (injectWith defaultInterpretOptions) (def :: MI.Manifest)

generateDefaultConfigurations :: Text -> IO ()
generateDefaultConfigurations prefix = do
  putText "Codegen: Dhall types."
  typeToFile (Proxy :: Proxy CPD.Values.Measurements) $ toS prefix <> "/types/CPDMeasurements.dhall"
  typeToFile (Proxy :: Proxy CPD.Values.Actions) $ toS prefix <> "/types/CPDActions.dhall"
  --typeToFile (Proxy :: Proxy CPD.Core.Problem) $ toS prefix <> "/types/CPDProblem.dhall"
  for_ [minBound .. maxBound] $ \t -> do
    let dest = toS prefix <> typeFile t
    putText $ "  Writing type for " <> show t <> " to " <> toS dest
    createDirectoryIfMissing True (takeDirectory dest)
    writeOutput licenseDhall dest (dhallType t)
  putText "Codegen: Dhall defaults."
  for_ [minBound .. maxBound] $ \defaultType -> do
    let dest = toS prefix <> defaultFile defaultType
    putStrLn $ "  Writing default for " <> show defaultType <> " to " <> dest <> "."
    createDirectoryIfMissing True (takeDirectory dest)
    writeOutput licenseDhall dest (Lint.lint $ getDefault defaultType)
  putText "Codegen: Manifest examples."
  for_ (DM.toList (Examples.examples :: Map Text MI.Manifest)) $ \(defName, defValue) -> do
    let dest = toS prefix <> "examples/" <> defName <> ".dhall"
    putText $ "  Writing default for " <> defName <> " to " <> dest <> "."
    createDirectoryIfMissing True (takeDirectory $ toS dest)
    writeOutput licenseDhall (toS dest)
      (Lint.lint $ Dhall.absurd <$> embed (injectWith defaultInterpretOptions) defValue)
  putText "Codegen: YAMl example files."
  for_ [minBound .. maxBound] $ \t -> do
    let yaml = yamlType t
        dest = toS prefix <> yamlFile t
    putText $ "  Writing yaml for " <> show t <> " to " <> toS dest
    createDirectoryIfMissing True (takeDirectory dest)
    writeFile dest $ licenseYaml <> toS yaml

typeToFile :: Interpret x => Proxy x -> String -> IO ()
typeToFile (Proxy :: Proxy x) fp = do
  let destCPD = fp
  putText $ "  Writing types for CPD format. " <> " to " <> toS destCPD
  createDirectoryIfMissing True (takeDirectory destCPD)
  writeOutput licenseDhall destCPD
    ( fmap Dhall.absurd
      ( Dhall.expected (Dhall.auto :: Dhall.Type x)
      )
    )
