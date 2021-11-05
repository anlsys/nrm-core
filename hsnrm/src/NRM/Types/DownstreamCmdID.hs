{-# LANGUAGE DerivingVia #-}

-- |
-- Module      : NRM.Types.DownstreamCmdID
-- Copyright   : (c) UChicago Argonne, 2019
-- License     : BSD3
-- Maintainer  : swann@anl.gov
module NRM.Types.DownstreamCmdID
  ( DownstreamCmdID (..),
    fromText,
    toText,
  )
where

import Data.Aeson
import Data.Data
import Data.JSON.Schema
import Data.MessagePack
import Data.String (IsString (..))
import qualified Data.UUID as U
import Protolude

newtype DownstreamCmdID = DownstreamCmdID U.UUID
  deriving (Show, Eq, Ord, Generic, Data, Read, ToJSONKey, FromJSONKey)

toText :: DownstreamCmdID -> Text
toText (DownstreamCmdID u) = U.toText u

fromText :: Text -> Maybe DownstreamCmdID
fromText = fmap DownstreamCmdID <$> U.fromText

instance IsString DownstreamCmdID where
  fromString x =
    fromMaybe
      (panic "couldn't decode DownstreamCmdID")
      (Data.Aeson.decode $ toS x)

instance ToJSON DownstreamCmdID where
  toJSON (DownstreamCmdID x) = toJSON x

instance FromJSON DownstreamCmdID where
  parseJSON = fmap DownstreamCmdID <$> parseJSON

instance JSONSchema DownstreamCmdID where
  schema Proxy = schema (Proxy :: Proxy Text)

instance MessagePack DownstreamCmdID where

  toObject (DownstreamCmdID c) = toObject $ U.toText c

  fromObject x =
    fromObject x >>= \y ->
      case DownstreamCmdID <$> U.fromText y of
        Nothing -> panic "Couldn't parse DownstreamCmdID"
        Just t -> return t
