{-|
Module      : NRM.Optparse.Client
Copyright   : (c) UChicago Argonne, 2019
License     : BSD3
Maintainer  : fre@freux.fr
-}
module NRM.Optparse.Client
  ( opts
  , Opts (..)
  , Listen (..)
  , ClientVerbosity (..)
  , CommonOpts (..)
  )
where

import qualified Data.ByteString as B
  ( getContents
  )
import LMap.Map as LM
import Data.Default
import Dhall
import qualified NRM.Types.Cmd as Cmd
import qualified NRM.Types.Configuration as Cfg
import NRM.Types.Manifest
import qualified NRM.Types.Manifest.Dhall as D
import qualified NRM.Types.Manifest.Yaml as Y
import NRM.Types.Messaging.UpstreamReq
import NRM.Types.Slice
import qualified NRM.Types.Units as U
import Options.Applicative
import Protolude hiding (All)
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
      , pubPort :: Int
      , rpcPort :: Int
      , upstreamBindAddress :: Text
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
      (long "json" <> short 'j' <> help "Enable json printing.") <*>
    Options.Applicative.option Options.Applicative.auto
      ( long "pub_port" <>
        metavar "PORT" <>
        help ("upstream pub port (default " <> show pub <> ").") <>
        value pub
      ) <*>
    Options.Applicative.option
      Options.Applicative.auto
      ( long "rpc_port" <>
        metavar "PORT" <>
        help ("upstream rpc port (default " <> show rpc <> ").") <>
        value rpc
      ) <*>
    Options.Applicative.strOption
      ( long "bind_address" <>
        metavar "ADDRESS" <>
        help ("upstream bind address (default " <> toS addr <> ").") <>
        value addr
      )
  where
    rpc = Cfg.rpcPort . Cfg.upstreamCfg $ def
    pub = Cfg.pubPort . Cfg.upstreamCfg $ def
    addr = "localhost" :: Text

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
        ( long "slice" <> short 's' <>
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

parserKillCmd :: Parser Cmd.CmdID
parserKillCmd =
  fromMaybe (panic "Couldn't parse CmdID") . Cmd.fromText <$>
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

data Listen = All | CPDOnly | Raw

data Opts = Opts {what :: Either Listen Req, commonOpts :: CommonOpts}

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
            ( Right . ReqKillCmd . KillCmd <$>
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
            ( Right . ReqKillSlice . KillSlice <$>
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
          ( Opts <$> (Right . ReqSetPower . SetPower <$> parserSetpower) <*>
            parserCommon
          )
        ) $
        progDesc "Set power limit"
      ) <>
    command
      "cpd"
      ( info (return <$> (Opts (Right $ ReqCPD CPD) <$> parserCommon)) $
        progDesc "Show current CPD"
      ) <>
    command
      "list"
      ( info (return <$> (Opts (Right $ ReqSliceList SliceList) <$> parserCommon)) $
        progDesc "List existing slices"
      ) <>
    command
      "state"
      ( info (return <$> (Opts (Right $ ReqGetState GetState) <$> parserCommon)) $
        progDesc "Show NRM state"
      ) <>
    command
      "config"
      ( info (return <$> (Opts (Right $ ReqGetConfig GetConfig) <$> parserCommon)) $
        progDesc "Show NRM configuration"
      ) <>
    command
      "listen-raw"
      ( info (return <$> (Opts (Left Raw) <$> parserCommon)) $
        progDesc "Listen to raw NRM upstream pub messages"
      ) <>
    command
      "listen-cpd"
      ( info (return <$> (Opts (Left CPDOnly) <$> parserCommon)) $
        progDesc "Listen to CPD messages"
      ) <>
    command
      "listen-all"
      ( info (return <$> (Opts (Left All) <$> parserCommon)) $
        progDesc "Listen to all upstream pub messages"
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
      ( Right . ReqRun $ Run
        { manifest = manifest
        , spec = Cmd.CmdSpec
          { cmd = Cmd.Command $ cmd rc
          , args = Cmd.Arguments $ Cmd.Arg <$> runargs rc
          , env = Cmd.Env $ LM.fromList env
          }
        , detachCmd = detach rc
        , runSliceID = cn
        }
      )
      common
