{-|
Module      : Nrm.Types.Client
Copyright   : (c) UChicago Argonne, 2019
License     : BSD3
Maintainer  : fre@freux.fr
-}
module Nrm.Types.Client
  ( ClientUUID (..)
  , nextClientUUID
  , parseClientUUID
  , toText
  , ClientVerbosity (..)
  )
where

import Data.Aeson
import Data.JSON.Schema
import Data.MessagePack
import qualified Data.UUID as U
import Data.UUID.V1
import Generics.Generic.Aeson
import Protolude
import Prelude (fail)

newtype ClientUUID = ClientUUID U.UUID
  deriving (Show, Eq, Ord, Generic)

nextClientUUID :: IO (Maybe ClientUUID)
nextClientUUID = fmap ClientUUID <$> nextUUID

data ClientVerbosity = Normal | Verbose
  deriving (Eq, Show)

parseClientUUID :: Text -> Maybe ClientUUID
parseClientUUID = fmap ClientUUID <$> U.fromText

toText :: ClientUUID -> Text
toText (ClientUUID u) = U.toText u

instance ToJSON ClientUUID where

  toJSON = gtoJson

instance FromJSON ClientUUID where

  parseJSON = gparseJson

instance JSONSchema ClientUUID where

  schema Proxy = schema (Proxy :: Proxy Text)

instance MessagePack ClientUUID where

  toObject (ClientUUID c) = toObject $ U.toText c

  fromObject x =
    fromObject x >>= \y ->
      case parseClientUUID y of
        Nothing -> fail "Couldn't parse ClientUUID"
        Just t -> return t
