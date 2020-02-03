{-# LANGUAGE DeriveAnyClass #-}
{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE DerivingStrategies #-}
{-# LANGUAGE DerivingVia #-}
{-# LANGUAGE LambdaCase #-}
{-# LANGUAGE NoImplicitPrelude #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE QuasiQuotes #-}
{-# LANGUAGE RecordWildCards #-}

import Control.Lens
import Data.Aeson
import Data.Aeson.Lens
import Data.Aeson.Types
import Data.Default
import NeatInterpolation
import Network.Connection (TLSSettings (..))
import Network.HTTP.Client.TLS (mkManagerSettings)
import Network.Wreq
import Options.Applicative as OA
import Protolude hiding (pass)
import qualified System.IO as SIO
  ( BufferMode (..),
    hSetBuffering,
    stdout,
  )

httpOpts :: CommonOpts -> Network.Wreq.Options
httpOpts (CommonOpts (User u) (Password p) _) =
  defaults
    & (auth ?~ basicAuth (toS u) (toS p))
    & (Network.Wreq.header "Accept" .~ ["*/*"])
    & manager
    .~ Left (mkManagerSettings (TLSSettingsSimple False False False) Nothing)

data ReservationRequestCLI
  = ReservationRequestCLI
      { nodes :: Nodes,
        walltime :: Walltime
      }
  deriving (Show, Generic)

data ReservationRequest
  = ReservationRequest
      { resources :: Text,
        command :: Text,
        types :: [Text]
      }
  deriving (Show, Generic, ToJSON)

data Hosts = Hosts [Text]
  deriving (Show, Generic, FromJSON, ToJSON)

newtype UID = UID Int
  deriving (Generic)
  deriving (Show, FromJSON, ToJSON) via Int

newtype Nodes = Nodes Int
  deriving (Show, Generic)
  deriving (ToJSON) via Int

newtype Walltime = Walltime Text
  deriving (Show, Generic)
  deriving (ToJSON) via Text

main :: IO ()
main = do
  SIO.hSetBuffering SIO.stdout SIO.NoBuffering
    <> void (join (execParser (info (parseOpts <**> helper) idm)))

hosts :: CommonOpts -> UID -> IO (Maybe Hosts)
hosts cOpts jobID = do
  r <-
    getWith (httpOpts cOpts) $
      "https://api.grid5000.fr/3.0/sites/rennes/jobs/"
        <> show jobID
        <> "?pretty"
  return $ (r ^? responseBody . key "assigned_nodes") >>= parseMaybe parseJSON

reserve :: CommonOpts -> ReservationRequestCLI -> IO (Maybe UID)
reserve cOpts@(CommonOpts _ _ (Site s)) (ReservationRequestCLI (Nodes n) (Walltime w)) = do
  let req = (ReservationRequest ("nodes=" <> show n <> ",walltime=" <> w) "sleep 100000" ["deploy"])
  r <-
    postWith
      (httpOpts cOpts)
      ("https://api.grid5000.fr/3.0/sites/" <> toS s <> "/jobs")
      (toJSON req)
  let uid = UID . floor <$> (r ^? responseBody . key "uid" . _Number)
  print uid
  return uid

deploy :: CommonOpts -> Hosts -> IO (Bool)
deploy = undefined

allOperations :: CommonOpts -> ReservationRequestCLI -> IO ()
allOperations c r = do
  reserve c r >>= \case
    Nothing -> die "Reservation failure"
    Just (UID id) -> do
      putText $ "Reserved job ID " <> show id
      (prompt ". Try to deploy?" <&> (`elem` ["y", "Y"])) >>= \case
        False -> exitSuccess
        True -> tryDeploy
  where
    tryDeploy = putText "pa"

prompt :: Text -> IO Text
prompt xs = putText (xs <> " ") >> getLine

hostsAndPrint :: CommonOpts -> UID -> IO ()
hostsAndPrint c i = hosts c i >>= \case
  Nothing -> die "Query failure"
  Just (hosts) -> putText $ "job has hosts " <> show hosts

reserveAndPrint :: CommonOpts -> ReservationRequestCLI -> IO ()
reserveAndPrint c r = reserve c r >>= \case
  Nothing -> die "Reservation failure"
  Just (UID id) -> putText $ "Reserved job ID " <> show id

deployAndPrint :: CommonOpts -> Hosts -> IO ()
deployAndPrint c h = deploy c h >>= \case
  True -> putText $ "deployed to hosts" <> show h
  _ -> die "Deployment request failure"

parseOpts :: OA.Parser (IO ())
parseOpts =
  hsubparser $
    OA.command
      "reserve"
      ( info ((reserveAndPrint <$> parserCommon <*> parserReservationRequestCLI)) $
          progDesc "Reserve a node"
      )
      <> OA.command
        "deploy"
        ( info ((deployAndPrint <$> parserCommon <*> parserHosts)) $
            progDesc "Deploy to a reserved node"
        )
      <> OA.command
        "hosts"
        ( info
            ( (hostsAndPrint <$> parserCommon <*> parserUID)
            )
            $ progDesc "Query job hosts"
        )
      <> OA.command
        "all"
        ( info
            ( (allOperations <$> parserCommon <*> parserReservationRequestCLI)
            )
            $ progDesc "Deploy and reserve"
        )
      <> help
        "Choice of operation."

data CommonOpts
  = CommonOpts
      { user :: User,
        password :: Password,
        site :: Site
      }

newtype Site = Site Text deriving (Show)

newtype User = User Text

newtype Password = Password Text

parserUID :: OA.Parser UID
parserUID = UID <$> OA.argument auto (metavar "UID")

parserHosts :: OA.Parser Hosts
parserHosts = Hosts <$> OA.argument auto (metavar "HOSTS")

parserReservationRequestCLI :: OA.Parser ReservationRequestCLI
parserReservationRequestCLI =
  ReservationRequestCLI
    <$> ( Nodes
            <$> OA.option auto (metavar "nodes" <> long "nodes" <> value 1)
        )
    <*> ( Walltime
            <$> OA.strOption (metavar "walltime" <> long "walltime" <> value "00:10")
        )

parserCommon :: OA.Parser CommonOpts
parserCommon =
  CommonOpts
    <$> ( User
            <$> strOption
              ( metavar "USER"
                  <> help "g5k username"
                  <> long "user"
                  <> short 'u'
              )
        )
    <*> ( Password
            <$> strOption
              ( metavar "PASSWORD"
                  <> help "g5k password"
                  <> long "pass"
                  <> short 'p'
              )
        )
    <*> ( Site
            <$> strOption
              ( long "site"
                  <> metavar "SITE"
                  <> help ("g5k site. default: rennes")
                  <> value "rennes"
                  <> short 's'
              )
        )
