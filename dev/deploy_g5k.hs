{-# LANGUAGE DeriveAnyClass #-}
{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE DerivingVia #-}
{-# LANGUAGE LambdaCase #-}
{-# LANGUAGE NoImplicitPrelude #-}
{-# LANGUAGE OverloadedStrings #-}

import Control.Lens
import Data.Aeson
import Data.Aeson.Lens as A
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

------ COMMON
newtype UID = UID Int
  deriving (Generic)
  deriving (Show, FromJSON, ToJSON) via Int

newtype Walltime = Walltime Text
  deriving (Show, Generic)
  deriving (ToJSON) via Text

newtype Site = Site Text
  deriving (Show)
  deriving (ToJSON) via Text

newtype Environment = Environment Text
  deriving (Show)
  deriving (ToJSON) via Text

newtype Pubfile = Pubfile Text
  deriving (Show)
  deriving (ToJSON) via Text

newtype Pubkey = Pubkey Text
  deriving (Show)
  deriving (ToJSON) via Text

newtype Hosts = Hosts [Text]
  deriving (Show, Generic)
  deriving (ToJSON, FromJSON) via [Text]

newtype User = User Text

newtype Password = Password Text

------ CLI
newtype NodeCount = NodeCount Int
  deriving (Show, Generic)
  deriving (ToJSON) via Int

data CommonOpts
  = CommonOpts
      { user :: User,
        password :: Password,
        site :: Site
      }

data ReservationCLI
  = ReservationCLI
      { nodecount :: NodeCount,
        walltime :: Walltime
      }

data DeployCLI
  = DeployCLI
      { deployHosts :: Hosts,
        pubfile :: Pubfile,
        envir :: Environment
      }
  deriving (Show)

data AllCLI
  = AllCLI
      { reservationCLI :: ReservationCLI,
        allPubfile :: Pubfile,
        allEnvironment :: Environment
      }

------ REQUESTS
data ReservationRequest
  = ReservationRequest
      { resources :: Text,
        command :: Text,
        types :: [Text]
      }
  deriving (Show, Generic, ToJSON)

data DeployRequest
  = DeployRequest
      { nodes :: Hosts,
        environment :: Environment,
        key :: Pubkey
      }
  deriving (Show, Generic, ToJSON)

httpOpts :: CommonOpts -> Network.Wreq.Options
httpOpts (CommonOpts (User u) (Password p) _) =
  defaults
    & (auth ?~ basicAuth (toS u) (toS p))
    & (Network.Wreq.header "Accept" .~ ["*/*"])
    & manager
    .~ Left (mkManagerSettings (TLSSettingsSimple False False False) Nothing)

main :: IO ()
main =
  SIO.hSetBuffering SIO.stdout SIO.NoBuffering
    <> void (join (execParser (info (parseOpts <**> helper) idm)))

hosts :: CommonOpts -> UID -> IO (Maybe Hosts)
hosts cOpts@(CommonOpts _ _ (Site s)) jobID = do
  r <-
    getWith (httpOpts cOpts) $
      "https://api.grid5000.fr/3.0/sites/"
        <> toS s
        <> "/jobs/"
        <> show jobID
        <> "?pretty"
  return $ (r ^? responseBody . A.key "assigned_nodes") >>= parseMaybe parseJSON

reserve :: CommonOpts -> ReservationCLI -> IO (Maybe UID)
reserve cOpts@(CommonOpts _ _ (Site s)) (ReservationCLI (NodeCount n) (Walltime w)) = do
  let req = ReservationRequest ("nodes=" <> show n <> ",walltime=" <> w) "sleep 100000" ["deploy"]
  r <-
    postWith
      (httpOpts cOpts)
      ("https://api.grid5000.fr/3.0/sites/" <> toS s <> "/jobs")
      (toJSON req)
  let uid = UID . floor <$> (r ^? responseBody . A.key "uid" . _Number)
  return uid

deploy :: CommonOpts -> DeployCLI -> IO Bool
deploy cOpts@(CommonOpts _ _ (Site s)) (DeployCLI hosts (Pubfile pubkey) environment) = do
  print hosts
  key <- Pubkey <$> readFile (toS pubkey)
  let req = DeployRequest {nodes = hosts, environment = environment, key = key}
  putText $ toS (encode req)
  r <- postWith (httpOpts cOpts) ("https://api.grid5000.fr/3.0/sites/" <> toS s <> "/deployments") (toJSON req)
  if r ^. responseStatus . statusCode == 201 then return True else print r >> return False

allOperations :: CommonOpts -> AllCLI -> IO ()
allOperations c r =
  reserve c (reservationCLI r) >>= \case
    Nothing -> die "Reservation failure"
    Just uid -> do
      putText $ "Reserved job " <> show uid
      tryHosts uid
  where
    tryHosts :: UID -> IO ()
    tryHosts jobid =
      (prompt "Try to get hosts? [y/Y]" <&> (`elem` ["y", "Y"])) >>= \case
        False -> exitSuccess
        True ->
          hosts c jobid >>= \case
            Nothing -> putText "no hosts associated yet" >> tryHosts jobid
            Just hs -> tryDeploy hs
    tryDeploy hs =
      (prompt "Try to deploy? [y/Y]" <&> (`elem` ["y", "Y"])) >>= \case
        False -> exitSuccess
        True ->
          deploy c (DeployCLI {deployHosts = hs, pubfile = allPubfile r, envir = allEnvironment r}) >>= \case
            False -> putText "deployment couldn't succeed.. " >> tryDeploy hs
            True -> putText "deployment success" >> exitSuccess

prompt :: Text -> IO Text
prompt xs = putText (xs <> " ") >> getLine

hostsAndPrint :: CommonOpts -> UID -> IO ()
hostsAndPrint c i =
  hosts c i >>= \case
    Nothing -> die "Query failure"
    Just (Hosts hosts) -> putText $ "job has hosts " <> show hosts

reserveAndPrint :: CommonOpts -> ReservationCLI -> IO ()
reserveAndPrint c r =
  reserve c r >>= \case
    Nothing -> die "Reservation failure"
    Just (UID id) -> putText $ "Reserved job ID " <> show id

deployAndPrint :: CommonOpts -> DeployCLI -> IO ()
deployAndPrint c d =
  deploy c d >>= \case
    True -> putText $ "deployed CLI deployment request" <> show d
    _ -> die "Deployment request failure"

parseOpts :: OA.Parser (IO ())
parseOpts =
  hsubparser $
    OA.command
      "reserve"
      ( info (reserveAndPrint <$> parserCommon <*> parserReservationCLI) $
          progDesc "Reserve a node"
      )
      <> OA.command
        "deploy"
        ( info (deployAndPrint <$> parserCommon <*> parserDeployCLI) $
            progDesc "Deploy to a reserved node"
        )
      <> OA.command
        "hosts"
        ( info
            ( hostsAndPrint <$> parserCommon <*> parserUID
            )
            $ progDesc "Query job hosts"
        )
      <> OA.command
        "all"
        ( info
            ( allOperations <$> parserCommon <*> (AllCLI <$> parserReservationCLI <*> parserPubfile <*> parserEnvironment)
            )
            $ progDesc "Deploy and reserve"
        )
      <> help
        "Choice of operation."

parserUID :: OA.Parser UID
parserUID = UID <$> OA.argument auto (metavar "UID")

parserHosts :: OA.Parser Hosts
parserHosts = Hosts <$> OA.option auto (metavar "HOSTS" <> long "hosts")

parserPubfile :: OA.Parser Pubfile
parserPubfile = Pubfile <$> OA.strOption (metavar "PUBKEY" <> long "pubfile" <> short 'k')

parserEnvironment :: OA.Parser Environment
parserEnvironment = Environment <$> OA.strOption (metavar "ENVIRONMENT" <> value "http://public.nancy.grid5000.fr/~orichard/nixos-19.09.yaml" <> long "environment" <> short 'e')

parserDeployCLI :: OA.Parser DeployCLI
parserDeployCLI = DeployCLI <$> parserHosts <*> parserPubfile <*> parserEnvironment

parserReservationCLI :: OA.Parser ReservationCLI
parserReservationCLI =
  ReservationCLI
    <$> ( NodeCount
            <$> OA.option auto (metavar "nodes" <> long "nodes" <> value 1)
        )
    <*> ( Walltime
            <$> OA.strOption (metavar "walltime" <> long "walltime" <> value "01:00")
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
                  <> help "g5k site. default: rennes"
                  <> value "rennes"
                  <> short 's'
              )
        )
