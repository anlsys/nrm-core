{-# LANGUAGE DeriveAnyClass #-}
{-# LANGUAGE DerivingStrategies #-}
{-# LANGUAGE DerivingVia #-}
{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE LambdaCase #-}
{-# LANGUAGE NoImplicitPrelude #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE QuasiQuotes #-}

import Control.Lens
import Data.Aeson
import Data.Aeson.Lens
import Data.Aeson.Types
import NeatInterpolation
import Network.Connection (TLSSettings (..))
import Network.HTTP.Client.TLS (mkManagerSettings)
import Network.Wreq
import Protolude hiding (pass)

opts =
  defaults &
    (auth ?~ basicAuth user pass) &
    (header "Accept" .~ ["*/*"]) &
    manager .~
    Left (mkManagerSettings (TLSSettingsSimple False False False) Nothing)

site = "rennes"

user = "valreis"

pass = "Equeum9O!"

data ReservationRequest
  = ReservationRequest
      { resources :: Text
      , command :: Text
      , types :: [Text]
      }
  deriving (Show, Generic, ToJSON)

data Hosts = Hosts [Text]
  deriving (Show, Generic, FromJSON, ToJSON)

newtype UID = UID Int
  deriving (Generic)
  deriving (Show, FromJSON, ToJSON) via Int

main :: IO ()
main = do
  reserve (ReservationRequest "nodes=1,walltime=00:10" "sleep 100000" ["deploy"]) >>= \case
    Nothing -> die "couldn't run reservation request"
    Just uid -> status uid >>= print

status :: UID -> IO (Maybe Hosts)
status jobID = do
  r <-
    getWith opts $ "https://api.grid5000.fr/3.0/sites/rennes/jobs/" <>
      show jobID <>
      "?pretty"
  return $ (r ^? responseBody . key "assigned_nodes") >>= parseMaybe parseJSON

reserve :: ReservationRequest -> IO (Maybe UID)
reserve request = do
  r <-
    postWith
      opts
      ("https://api.grid5000.fr/3.0/sites/" <> site <> "/jobs")
      (toJSON request)
  let uid = UID . floor <$> (r^? responseBody . key "uid" . _Number )
  print uid
  return uid
  --return $ r ^? responseBody . key "uid" 
