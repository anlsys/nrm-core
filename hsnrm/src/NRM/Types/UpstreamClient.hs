-- |
-- Module      : NRM.Types.UpstreamClient
-- Copyright   : (c) UChicago Argonne, 2019
-- License     : BSD3
-- Maintainer  : swann@anl.gov
module NRM.Types.UpstreamClient
  ( UpstreamClientID (..),
    nextUpstreamClientID,
    toText,
    fromText,
  )
where

import Data.Aeson
import Data.Data
import Data.JSON.Schema
import Data.MessagePack
import qualified Data.UUID as U
import Data.UUID.V1
import Protolude

newtype UpstreamClientID = UpstreamClientID U.UUID
  deriving (Show, Eq, Ord, Generic, Data, Read)

nextUpstreamClientID :: IO (Maybe UpstreamClientID)
nextUpstreamClientID = fmap UpstreamClientID <$> nextUUID

toText :: UpstreamClientID -> Text
toText (UpstreamClientID u) = U.toText u

fromText :: Text -> Maybe UpstreamClientID
fromText = fmap UpstreamClientID <$> U.fromText

instance ToJSON UpstreamClientID where
  toJSON (UpstreamClientID x) = toJSON x

instance FromJSON UpstreamClientID where
  parseJSON = fmap UpstreamClientID <$> parseJSON

instance JSONSchema UpstreamClientID where
  schema Proxy = schema (Proxy :: Proxy Text)

instance MessagePack UpstreamClientID where

  toObject (UpstreamClientID c) = toObject $ U.toText c

  fromObject x =
    fromObject x >>= \y ->
      case UpstreamClientID <$> U.fromText y of
        Nothing -> panic "Couldn't parse UpstreamClientID"
        Just t -> return t
