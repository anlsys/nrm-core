-- |
-- Module      : NRM.Optparse.Client
-- Copyright   : (c) UChicago Argonne, 2019
-- License     : BSD3
-- Maintainer  : fre@freux.fr
module NRM.Optparse.Client
  ( opts,
    Opts (..),
    Listen (..),
    ClientVerbosity (..),
    CommonOpts (..),
  )
where

import CPD.Core as CPD
import CPD.Values as CPD
import qualified Data.ByteString as B
  ( getContents,
  )
import Data.Default
import Data.MessagePack
import Dhall
import LMap.Map as LM
import NRM.Optparse.Daemon hiding (opts)
import qualified NRM.Types.Cmd as Cmd
import NRM.Types.CmdID
import qualified NRM.Types.Configuration as Cfg
import NRM.Types.Manifest
import NRM.Types.Messaging.UpstreamReq
import NRM.Types.Slice
import qualified NRM.Types.Units as U
import Options.Applicative
import Protolude hiding (All)
import System.Directory
import System.Environment

data CommonOpts
  = CommonOpts
      { verbose :: ClientVerbosity,
        jsonPrint :: Bool,
        color :: Bool,
        pubPort :: Int,
        rpcPort :: Int,
        upstreamBindAddress :: Text
      }
  deriving (Generic, MessagePack)

instance Default CommonOpts where
  def = CommonOpts Normal False False pub rpc addr
    where
      rpc = Cfg.rpcPort . Cfg.upstreamCfg $ def
      pub = Cfg.pubPort . Cfg.upstreamCfg $ def
      addr = "localhost" :: Text

data ClientVerbosity = Normal | Verbose
  deriving (Eq, Show, Generic, MessagePack)

parserCommon :: Parser CommonOpts
parserCommon =
  CommonOpts
    <$> flag
      Normal
      Verbose
      (long "verbose" <> short 'v' <> help "Enable verbose mode.")
    <*> flag
      False
      True
      (long "json" <> short 'j' <> help "Enable json printing.")
    <*> flag
      False
      True
      (long "color" <> short 'c' <> help "Enable color printing.")
    <*> Options.Applicative.option
      Options.Applicative.auto
      ( long "pub_port"
          <> metavar "PORT"
          <> help ("upstream pub port (default " <> show pub <> ").")
          <> value pub
      )
    <*> Options.Applicative.option
      Options.Applicative.auto
      ( long "rpc_port"
          <> metavar "PORT"
          <> help ("upstream rpc port (default " <> show rpc <> ").")
          <> value rpc
      )
    <*> Options.Applicative.strOption
      ( long "bind_address"
          <> metavar "ADDRESS"
          <> help ("upstream bind address (default " <> toS addr <> ").")
          <> value addr
      )
  where
    pub = pubPort def
    rpc = rpcPort def
    addr = upstreamBindAddress def

data RunCfg
  = RunCfg
      { useStdin :: Bool,
        stdinType :: SourceType,
        detach :: Bool,
        inputfile :: Maybe Text,
        sliceName :: Maybe Text,
        cmd :: Text,
        runargs :: [Text]
      }

parserRun :: Parser RunCfg
parserRun =
  RunCfg
    <$> flag
      False
      True
      (long "stdin" <> short 'i' <> help "Read configuration on stdin.")
    <*> flag
      Dhall
      Json
      ( long "yaml" <> short 'y'
          <> help
            "Assume stdin to be yaml(json is valid yaml) instead of dhall."
      )
    <*> flag
      False
      True
      (long "detach" <> short 'd' <> help "Detach the command.")
    <*> optional
      ( strOption
          ( long "manifest"
              <> metavar "MANIFEST"
              <> help
                "Input manifest with .yml/.yaml/.dh/.dhall extension."
          )
      )
    <*> optional
      ( strOption
          ( long "slice" <> short 's'
              <> metavar "CONTAINER"
              <> help
                "Slice name/ID"
          )
      )
    <*> strArgument
      ( metavar "CMD"
          <> help
            "Command name"
      )
    <*> many
      ( strArgument
          ( metavar "ARG"
              <> help
                "Command arguments"
          )
      )

parserKillCmd :: Parser CmdID
parserKillCmd =
  fromMaybe (panic "Couldn't parse CmdID") . fromText
    <$> strArgument
      ( metavar "COMMAND"
          <> help
            "ID of the command to kill"
      )

parserKillSlice :: Parser SliceID
parserKillSlice =
  parseSliceID
    <$> strArgument
      ( metavar "CONTAINER"
          <> help
            "Name/ID of the slice to kill"
      )

parserActuate :: Parser [CPD.Action]
parserActuate =
  (\x -> [x])
    <$> ( CPD.Action
            <$> parserActuatorID
            <*> parserActuatorValue
        )

parserActuatorID :: Parser CPD.ActuatorID
parserActuatorID =
  ActuatorID
    <$> strArgument
      ( metavar "CONTAINER"
          <> help
            "Name/ID of the slice to kill"
      )

parserActuatorValue :: Parser CPD.Discrete
parserActuatorValue =
  CPD.DiscreteDouble
    <$> argument
      Options.Applicative.auto
      ( metavar "CONTAINER"
          <> help
            "Name/ID of the slice to kill"
      )

data Listen = All | CPDOnly | Raw

data Opts = Opts {what :: Either Listen Req, commonOpts :: CommonOpts}

opts :: Parser (IO Opts)
opts =
  hsubparser $
    command
      "run"
      ( info (run <$> parserRun <*> parserCommon) $
          progDesc "Run the application via NRM"
      )
      <> command
        "killcmd"
        ( info
            ( return
                <$> ( Opts
                        <$> ( Right . ReqKillCmd . KillCmd
                                <$> parserKillCmd
                            )
                        <*> parserCommon
                    )
            )
            $ progDesc "Kill cmd"
        )
      <> command
        "killslice"
        ( info
            ( return
                <$> ( Opts
                        <$> ( Right . ReqKillSlice . KillSlice
                                <$> parserKillSlice
                            )
                        <*> parserCommon
                    )
            )
            $ progDesc "Kill slice"
        )
      <> command
        "actuate"
        ( info
            ( return
                <$> ( Opts <$> (Right . ReqActuate <$> parserActuate)
                        <*> parserCommon
                    )
            )
            $ progDesc "Send actuator action"
        )
      <> command
        "cpd"
        ( info (return <$> (Opts (Right $ ReqCPD CPD) <$> parserCommon)) $
            progDesc "Show current CPD"
        )
      <> command
        "list"
        ( info (return <$> (Opts (Right $ ReqSliceList SliceList) <$> parserCommon)) $
            progDesc "List existing slices"
        )
      <> command
        "state"
        ( info (return <$> (Opts (Right $ ReqGetState GetState) <$> parserCommon)) $
            progDesc "Show NRM state"
        )
      <> command
        "config"
        ( info (return <$> (Opts (Right $ ReqGetConfig GetConfig) <$> parserCommon)) $
            progDesc "Show NRM configuration"
        )
      <> command
        "listen-raw"
        ( info (return <$> (Opts (Left Raw) <$> parserCommon)) $
            progDesc "Listen to raw NRM upstream pub messages"
        )
      <> command
        "listen-cpd"
        ( info (return <$> (Opts (Left CPDOnly) <$> parserCommon)) $
            progDesc "Listen to CPD messages"
        )
      <> command
        "listen-all"
        ( info (return <$> (Opts (Left All) <$> parserCommon)) $
            progDesc "Listen to all upstream pub messages"
        )
      <> help
        "Choice of operation."

load :: RunCfg -> IO Manifest
load RunCfg {..} =
  case ext useStdin stdinType inputfile of
    UseDefault -> return def
    (FinallyFile sourceType filename) ->
      makeAbsolute (toS filename) >>= readFile >>= (process sourceType . toS)
    (FinallyStdin sourceType) ->
      B.getContents >>= process sourceType
    NoExt -> inputfile & \case
      Nothing -> return def
      Just s -> process stdinType (toS s)
  where
    process = processType (Proxy :: Proxy Manifest)

run :: RunCfg -> CommonOpts -> IO Opts
run rc common = do
  manifest <- load rc
  cn <-
    case sliceName rc of
      Nothing -> fromMaybe (panic "Couldn't generate next slice ID") <$> nextSliceID
      Just n -> return $ Name n
  env <- fmap (bimap toS toS) <$> getEnvironment
  return $
    Opts
      ( Right . ReqRun $ Run
          { manifest = manifest,
            spec = Cmd.CmdSpec
              { cmd = Cmd.Command $ cmd rc,
                args = Cmd.Arguments $ Cmd.Arg <$> runargs rc,
                env = Cmd.Env $ LM.fromList env
              },
            detachCmd = detach rc,
            runSliceID = cn
          }
      )
      common
