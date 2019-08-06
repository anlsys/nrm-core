{-|
Module      : Nrm.Argparse.Client
Description : Client argument parsing
Copyright   : (c) UChicago Argonne, 2019
License     : BSD3
Maintainer  : fre@freux.fr
-}
module Nrm.Argparse.Client
  ( parseCli
  )
where

import qualified Data.ByteString as B
  ( getContents
  )
import Dhall
import GHC.IO.Encoding
import qualified Nrm.Types.Manifest.Dhall as D
import Nrm.Types.Manifest.Internal
import qualified Nrm.Types.Manifest.Yaml as Y
import Options.Applicative
import Protolude
import System.Directory
import System.FilePath.Posix
import qualified System.IO as SIO
import Text.Editor
import qualified Prelude
  ( print
  )

parseCli :: IO Manifest
parseCli =
  GHC.IO.Encoding.setLocaleEncoding SIO.utf8 >>
    ( join .
      customExecParser (prefs showHelpOnError) $
      info
        (helper <*> opts)
        ( fullDesc <> header "dhrun" <>
          progDesc
            ( "dhall-configured concurrent process execution" <>
              " with streaming assertion monitoring"
            )
        )
    )

data MainCfg
  = MainCfg
      { inputfile :: Maybe Text
      , stdinType :: SourceType
      , verbosity :: ClientVerbosity
      , edit :: Bool
      }

commonParser :: Parser MainCfg
commonParser =
  MainCfg <$>
    optional
      ( strArgument
        ( metavar "INPUT" <>
          help
            "Input configuration with .yml/.yaml/.dh/.dhall extension. Leave void for stdin (dhall) input."
        )
      ) <*>
    flag
      Dhall
      Yaml
      ( long "yaml" <> short 'y' <>
        help
          "Assume stdin to be yaml instead of dhall."
      ) <*>
    flag Normal
      Verbose
      (long "verbose" <> short 'v' <> help "Enable verbose mode.") <*>
    flag
      False
      True
      (long "edit" <> short 'e' <> help "Edit yaml in $EDITOR before running the NRM daemon.")

opts :: Parser (IO Manifest)
opts =
  hsubparser $
    command
      "run"
      (info (load <$> commonParser) $ progDesc "Run the NRM daemon") <>
    command
      "print"
      ( info (printY <$> commonParser) $
        progDesc "Just print the daemon configuration (don't run the NRM daemon)."
      ) <>
    help "Choice of operation."

data SourceType = Dhall | Yaml
  deriving (Eq)

data FinallySource = NoExt | FinallyFile SourceType Text | FinallyStdin SourceType

ext :: SourceType -> Maybe Text -> FinallySource
ext _ (Just fn)
  | xt `elem` [".dh", ".dhall"] = FinallyFile Dhall fn
  | xt `elem` [".yml", ".yaml"] = FinallyFile Yaml fn
  | otherwise = NoExt
  where
    xt = takeExtension $ toS fn
ext st Nothing = FinallyStdin st

load :: MainCfg -> IO Manifest
load MainCfg {..} =
  (if edit then editing else return) =<< case ext stdinType inputfile of
    (FinallyFile Dhall filename) ->
      (if v then detailed else identity) $
        D.inputManifest =<<
        toS <$>
        makeAbsolute (toS filename)
    (FinallyFile Yaml filename) ->
      Y.decodeManifestFile =<< toS <$> makeAbsolute (toS filename)
    (FinallyStdin Yaml) ->
      B.getContents <&> Y.decodeManifest >>= \case
        Left e -> Prelude.print e >> die "yaml parsing exception."
        Right manifest -> return manifest
    (FinallyStdin Dhall) -> B.getContents >>= D.inputManifest . toS
    NoExt ->
      die
        ( "couldn't figure out extension for input file. " <>
          "Please use something in {.yml,.yaml,.dh,.dhall} ."
        )
  where
    v = verbosity == Verbose

editing :: Manifest -> IO Manifest
editing c =
  runUserEditorDWIM yt (Y.encodeManifest c) <&> Y.decodeManifest >>= \case
    Left e -> Prelude.print e >> die "yaml parsing exception."
    Right manifest -> return manifest
  where
    yt = mkTemplate "yaml"

printY :: MainCfg -> IO Manifest
printY c = do
  manifest <- load c
  putText . toS . Y.encodeManifest $ manifest
  return manifest
