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

import Nrm.Types.Configuration (Cfg)
import Options.Applicative
import Protolude

parseCli :: IO (Maybe Cfg)
parseCli = do
  GHC.IO.Encoding.setLocaleEncoding SIO.utf8
  cfg <-
    join . customExecParser (prefs showHelpOnError) $
      info
        (helper <*> opts)
        ( fullDesc <> header "dhrun" <>
          progDesc
            ( "dhall-configured concurrent process execution" <>
              " with streaming assertion monitoring"
            )
        )
  return cfg

data MainCfg
  = MainCfg
      { inputfile :: Maybe Text
      , stdinType :: SourceType
      , verbosity :: Verbosity
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

opts :: Parser (IO ())
opts =
  hsubparser $
    command
      "run"
      (info (run <$> commonParser) $ progDesc "Run the NRM daemon") <>
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
  (if edit then editing else return) =<<
    overrideV <$> case ext stdinType inputfile of
    (FinallyFile Dhall filename) ->
      (if v then detailed else identity) $
        inputCfg =<<
        toS <$>
        makeAbsolute (toS filename)
    (FinallyFile Yaml filename) ->
      decodeCfgFile =<< toS <$> makeAbsolute (toS filename)
    (FinallyStdin Yaml) ->
      B.getContents <&> decodeCfg >>= \case
        Left e -> Prelude.print e >> die "yaml parsing exception."
        Right cfg -> return cfg
    (FinallyStdin Dhall) -> B.getContents >>= inputCfg . toS
    NoExt ->
      die
        ( "couldn't figure out extension for input file. " <>
          "Please use something in {.yml,.yaml,.dh,.dhall} ."
        )
  where
    v = verbosity == Verbose
    overrideV x =
      x
        { DI.verbosity = if (DI.verbosity x == Verbose) || v
          then Verbose
          else Normal
        }

editing :: Cfg -> IO Cfg
editing c =
  runUserEditorDWIM yt (encodeCfg c) <&> decodeCfg >>= \case
    Left e -> Prelude.print e >> die "yaml parsing exception."
    Right cfg -> return cfg
  where
    yt = mkTemplate "yaml"

run :: MainCfg -> IO Cfg
run = load

printY :: MainCfg -> IO (Maybe Cfg)
printY c = (load c >>= putText . toS . encodeCfg) >> return Nothing
