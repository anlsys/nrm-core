{-# LANGUAGE DeriveAnyClass #-}
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

main :: IO ()
main = do
  let dummy = (ReservationRequest "nodes=1,walltime=02:00" "sleep 10" ["deploy"])
  status 404150 >>= print

status :: Int -> IO (Maybe Hosts)
status jobID = do
  r <-
    getWith opts $ "https://api.grid5000.fr/3.0/sites/rennes/jobs/" <>
      show jobID <>
      "?pretty"
  return $ (r ^? responseBody . key "assigned_nodes") >>= parseMaybe parseJSON

reserve :: ReservationRequest -> IO ()
reserve request = do
  r <-
    postWith
      opts
      ("https://api.grid5000.fr/3.0/sites/" <> site <> "/jobs")
      (toJSON request)
  print $ r
