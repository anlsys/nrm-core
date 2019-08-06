{-|
Module      : Nrm.Optparse.Daemon
Copyright   : (c) UChicago Argonne, 2019
License     : BSD3
Maintainer  : fre@freux.fr
-}
module Nrm.Optparse.Daemon
  ( opts
  )
where

import qualified Data.ByteString as B
  ( getContents
  )
import Dhall
import qualified Nrm.Types.Configuration.Dhall as D
import Nrm.Types.Configuration
import qualified Nrm.Types.Configuration.Yaml as Y
import Options.Applicative
import Protolude
import System.Directory
import System.FilePath.Posix
import Text.Editor
import qualified Prelude
  ( print
  )

data MainCfg
  = MainCfg
      { inputfile :: Maybe Text
      , stdinType :: SourceType
      , verbosity :: DaemonVerbosity
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

opts :: Parser (IO Cfg)
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

load :: MainCfg -> IO Cfg
load MainCfg {..} =
  (if edit then editing else return) =<< case ext stdinType inputfile of
    (FinallyFile Dhall filename) ->
      (if v then detailed else identity) $
        D.inputCfg =<<
        toS <$>
        makeAbsolute (toS filename)
    (FinallyFile Yaml filename) ->
      Y.decodeCfgFile =<< toS <$> makeAbsolute (toS filename)
    (FinallyStdin Yaml) ->
      B.getContents <&> Y.decodeCfg >>= \case
        Left e -> Prelude.print e >> die "yaml parsing exception."
        Right cfg -> return cfg
    (FinallyStdin Dhall) -> B.getContents >>= D.inputCfg . toS
    NoExt ->
      die
        ( "couldn't figure out extension for input file. " <>
          "Please use something in {.yml,.yaml,.dh,.dhall} ."
        )
  where
    v = verbosity == Verbose

editing :: Cfg -> IO Cfg
editing c =
  runUserEditorDWIM yt (Y.encodeCfg c) <&> Y.decodeCfg >>= \case
    Left e -> Prelude.print e >> die "yaml parsing exception."
    Right cfg -> return cfg
  where
    yt = mkTemplate "yaml"

printY :: MainCfg -> IO Cfg
printY c = do
  cfg <- load c
  putText . toS . Y.encodeCfg $ cfg
  return cfg
