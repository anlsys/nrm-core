{-# LANGUAGE DerivingVia #-}

{-|
Module      : NRM.Types.DownstreamCmdClient
Copyright   : (c) UChicago Argonne, 2019
License     : BSD3
Maintainer  : fre@freux.fr
-}
module NRM.Types.DownstreamCmdClient
  ( DownstreamCmdClientID (..)
  , DownstreamCmdClient (..)
  , fromText
  , toText
  , toSensorID
  )
where

import Data.Aeson
import Data.Data
import Data.JSON.Schema
import Data.MessagePack
import Data.String (IsString (..))
import qualified Data.UUID as U
import NRM.Classes.Messaging
import NRM.Types.Sensor
import NRM.Types.Units as Units
import Protolude
import Prelude (fail)

newtype DownstreamCmdClientID = DownstreamCmdClientID U.UUID
  deriving (Show, Eq, Ord, Generic, Data, Read, ToJSONKey, FromJSONKey)

toSensorID :: DownstreamCmdClientID -> SensorID
toSensorID (DownstreamCmdClientID uuid) = SensorID uuid

toText :: DownstreamCmdClientID -> Text
toText (DownstreamCmdClientID u) = U.toText u

fromText :: Text -> Maybe DownstreamCmdClientID
fromText = fmap DownstreamCmdClientID <$> U.fromText

data DownstreamCmdClient
  = DownstreamCmdClient
      { id :: SensorID
      , maxValue :: Units.Operations
      }
  deriving (Eq, Ord, Show, Generic, Data, MessagePack)
  deriving
    (JSONSchema, ToJSON, FromJSON)
    via GenericJSON DownstreamCmdClient

instance IsString DownstreamCmdClientID where

  fromString x =
    fromMaybe (panic "couldn't decode DownstreamCmdID")
      (Data.Aeson.decode $ toS x)

instance ToJSON DownstreamCmdClientID where

  toJSON (DownstreamCmdClientID x) = toJSON x

instance FromJSON DownstreamCmdClientID where

  parseJSON = fmap DownstreamCmdClientID <$> parseJSON

instance JSONSchema DownstreamCmdClientID where

  schema Proxy = schema (Proxy :: Proxy Text)

instance MessagePack DownstreamCmdClientID where

  toObject (DownstreamCmdClientID c) = toObject $ U.toText c

  fromObject x =
    fromObject x >>= \y ->
      case DownstreamCmdClientID <$> U.fromText y of
        Nothing -> fail "Couldn't parse DownstreamCmdClientID"
        Just t -> return t
