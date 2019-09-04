{-|
Module      : NRM.Optparse.Client
Copyright   : (c) UChicago Argonne, 2019
License     : BSD3
Maintainer  : fre@freux.fr
-}
module NRM.Optparse.Client
  ( opts
  , Opts (..)
  , ClientVerbosity (..)
  , CommonOpts (..)
  )
where

import qualified Data.ByteString as B
  ( getContents
  )
import Data.Default
import Dhall
import NRM.Types.Manifest
import qualified NRM.Types.Manifest.Dhall as D
import qualified NRM.Types.Manifest.Yaml as Y
import NRM.Types.Messaging.UpstreamReq
import qualified NRM.Types.Process as P
import NRM.Types.Slice
import qualified NRM.Types.Units as U
import Options.Applicative
import Protolude
import System.Directory
import System.Environment
import System.FilePath.Posix
import Text.Editor
import qualified Prelude
  ( print
  )

data CommonOpts
  = CommonOpts
      { verbose :: ClientVerbosity
      , jsonPrint :: Bool
      }

data ClientVerbosity = Normal | Verbose
  deriving (Eq, Show)

parserCommon :: Parser CommonOpts
parserCommon =
  CommonOpts <$>
    flag Normal
      Verbose
      (long "verbose" <> short 'v' <> help "Enable verbose mode.") <*>
    flag False
      True
      (long "json" <> short 'v' <> help "Enable json printing.")

data RunCfg
  = RunCfg
      { useStdin :: Bool
      , stdinType :: SourceType
      , detach :: Bool
      , edit :: Bool
      , inputfile :: Maybe Text
      , sliceName :: Maybe Text
      , cmd :: Text
      , runargs :: [Text]
      }

parserRun :: Parser RunCfg
parserRun =
  RunCfg <$>
    flag
      False
      True
      (long "stdin" <> short 'i' <> help "Read configuration on stdin.") <*>
    flag
      Dhall
      Yaml
      ( long "yaml" <> short 'y' <>
        help
          "Assume stdin to be yaml instead of dhall."
      ) <*>
    flag
      False
      True
      (long "detach" <> short 'd' <> help "Detach the command.") <*>
    flag
      False
      True
      (long "edit" <> short 'e' <> help "Edit manifest yaml in $EDITOR before running the NRM client.") <*>
    optional
      ( strOption
        ( long "manifest" <>
          metavar "MANIFEST" <>
          help
            "Input manifest with .yml/.yaml/.dh/.dhall extension."
        )
      ) <*>
    optional
      ( strOption
        ( long "slice" <> short 'c' <>
          metavar "CONTAINER" <>
          help
            "Slice name/ID"
        )
      ) <*>
    strArgument
      ( metavar "CMD" <>
        help
          "Command name"
      ) <*>
    many
      ( strArgument
        ( metavar "ARG" <>
          help
            "Command arguments"
        )
      )

parserKillCmd :: Parser P.CmdID
parserKillCmd =
  fromMaybe (panic "Couldn't parse CmdID") . P.fromText <$>
    strArgument
      ( metavar "COMMAND" <>
        help
          "ID of the command to kill"
      )

parserKillSlice :: Parser SliceID
parserKillSlice =
  parseSliceID <$>
    strArgument
      ( metavar "CONTAINER" <>
        help
          "Name/ID of the slice to kill"
      )

parserSetpower :: Parser U.Power
parserSetpower =
  U.watts <$>
    argument Options.Applicative.auto
      ( metavar "POWERLIMIT" <>
        help
          "Power limit to set"
      )

data Opts = Opts {req :: Req, commonOpts :: CommonOpts}

opts :: Parser (IO Opts)
opts =
  hsubparser $
    command "run"
      ( info (run <$> parserRun <*> parserCommon) $
        progDesc "Run the application via NRM"
      ) <>
    command "killcmd"
      ( info
        ( return <$>
          ( Opts <$>
            ( ReqKillCmd . KillCmd <$>
              parserKillCmd
            ) <*>
            parserCommon
          )
        ) $
        progDesc "Kill cmd"
      ) <>
    command "killslice"
      ( info
        ( return <$>
          ( Opts <$>
            ( ReqKillSlice . KillSlice <$>
              parserKillSlice
            ) <*>
            parserCommon
          )
        ) $
        progDesc "Kill slice"
      ) <>
    command
      "setpower"
      ( info
        ( return <$>
          ( Opts <$> (ReqSetPower . SetPower <$> parserSetpower) <*>
            parserCommon
          )
        ) $
        progDesc "Set power limit"
      ) <>
    command
      "cpd"
      ( info (return <$> (Opts (ReqCPD CPD) <$> parserCommon)) $
        progDesc "Show current CPD"
      ) <>
    command
      "list"
      ( info (return <$> (Opts (ReqSliceList SliceList) <$> parserCommon)) $
        progDesc "List existing slices"
      ) <>
    command
      "state"
      ( info (return <$> (Opts (ReqGetState GetState) <$> parserCommon)) $
        progDesc "Show NRM state"
      ) <>
    command
      "config"
      ( info (return <$> (Opts (ReqGetConfig GetConfig) <$> parserCommon)) $
        progDesc "Show NRM configuration"
      ) <>
    help
      "Choice of operation."

data SourceType = Dhall | Yaml
  deriving (Eq)

data FinallySource = UseDefault | NoExt | FinallyFile SourceType Text | FinallyStdin SourceType

ext :: Bool -> SourceType -> Maybe Text -> FinallySource
ext _ _ (Just fn)
  | xt `elem` [".dh", ".dhall"] = FinallyFile Dhall fn
  | xt `elem` [".yml", ".yaml"] = FinallyFile Yaml fn
  | otherwise = NoExt
  where
    xt = takeExtension $ toS fn
ext useStdin st Nothing = if useStdin then FinallyStdin st else UseDefault

load :: RunCfg -> IO Manifest
load RunCfg {..} =
  (if edit then editing else return) =<< case ext useStdin stdinType inputfile of
    (FinallyFile Dhall filename) ->
      detailed $
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
    UseDefault -> return def
    NoExt ->
      die
        ( "couldn't figure out extension for input file. " <>
          "Please use something in {.yml,.yaml,.dh,.dhall} ."
        )

editing :: Manifest -> IO Manifest
editing c =
  runUserEditorDWIM yt (Y.encodeManifest c) <&> Y.decodeManifest >>= \case
    Left e -> Prelude.print e >> die "yaml parsing exception."
    Right manifest -> return manifest
  where
    yt = mkTemplate "yaml"

run :: RunCfg -> CommonOpts -> IO Opts
run rc common = do
  manifest <- load rc
  cn <-
    case sliceName rc of
      Nothing -> fromMaybe (panic "Couldn't generate next slice ID") <$> nextSliceID
      Just n -> return $ Name n
  env <- fmap (\(x, y) -> (toS x, toS y)) <$> getEnvironment
  return $
    Opts
      ( ReqRun $ Run
        { manifest = manifest
        , spec = P.CmdSpec
          { cmd = P.Command $ cmd rc
          , args = P.Arguments $ P.Arg <$> runargs rc
          , env = P.Env env
          }
        , detachCmd = detach rc
        , runSliceID = cn
        }
      )
      common