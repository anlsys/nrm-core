{-|
Module      : Nrm.Types.Application
Copyright   : (c) UChicago Argonne, 2019
License     : BSD3
Maintainer  : fre@freux.fr
-}
module Nrm.Types.Application
  ( ApplicationUUID (..)
  , Arg (..)
  , Command (..)
  , Arguments (..)
  , AppStartConfig (..)
  , nextApplicationUUID
  , parseApplicationUUID
  , toText
  )
where

import qualified Data.Aeson as A
import Data.Aeson
import Data.JSON.Schema
import Data.MessagePack
import qualified Data.UUID as U (UUID, fromText, toText)
import Data.UUID.V1
import Generics.Generic.Aeson
import Protolude
import Prelude (fail)

newtype ApplicationUUID = ApplicationUUID U.UUID
  deriving (Eq, Ord, Show, Generic)

newtype Arg = Arg Text
  deriving (Show, Generic)

deriving instance MessagePack Arg

newtype Command = Command Text
  deriving (Show, Generic)

deriving instance MessagePack Command

newtype Arguments = Arguments [Arg]
  deriving (Show, Generic)

deriving instance MessagePack Arguments

data AppStartConfig
  = AppStartConfig
      { command :: Command
      , arguments :: Arguments
      , applicationUUID :: ApplicationUUID
      }

nextApplicationUUID :: IO (Maybe ApplicationUUID)
nextApplicationUUID = fmap ApplicationUUID <$> nextUUID

parseApplicationUUID :: Text -> Maybe ApplicationUUID
parseApplicationUUID = fmap ApplicationUUID <$> U.fromText

toText :: ApplicationUUID -> Text
toText (ApplicationUUID u) = U.toText u

instance ToJSON ApplicationUUID where

  toJSON = gtoJson

instance FromJSON ApplicationUUID where

  parseJSON = gparseJson

instance JSONSchema ApplicationUUID where

  schema Proxy = schema (Proxy :: Proxy Text)

instance ToJSON Command where

  toJSON = gtoJson

instance FromJSON Command where

  parseJSON = gparseJson

instance JSONSchema Command where

  schema = gSchema

instance ToJSON Arguments where

  toJSON = gtoJson

instance FromJSON Arguments where

  parseJSON = gparseJson

instance JSONSchema Arguments where

  schema = gSchema

instance ToJSON Arg where

  toJSON = gtoJson

instance FromJSON Arg where

  parseJSON = gparseJson

instance JSONSchema Arg where

  schema = gSchema

instance MessagePack ApplicationUUID where

  toObject (ApplicationUUID c) = toObject $ U.toText c

  fromObject x =
    fromObject x >>= \y ->
      case parseApplicationUUID y of
        Nothing -> fail "Couldn't parse ApplicationUUID"
        Just t -> return t
