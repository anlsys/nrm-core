{-|
Module      : Nrm.Types.UpstreamClient
Copyright   : (c) UChicago Argonne, 2019
License     : BSD3
Maintainer  : fre@freux.fr
-}
module Nrm.Types.UpstreamClient
  ( UpstreamClientID (..)
  , nextUpstreamClientID
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

newtype UpstreamClientID = UpstreamClientID U.UUID
  deriving (Show, Eq, Ord, Generic, Read)

nextUpstreamClientID :: IO (Maybe UpstreamClientID)
nextUpstreamClientID = fmap UpstreamClientID <$> nextUUID

data ClientVerbosity = Normal | Verbose
  deriving (Eq, Show)

toText :: UpstreamClientID -> Text
toText (UpstreamClientID u) = U.toText u

instance ToJSON UpstreamClientID where

  toJSON = gtoJson

instance FromJSON UpstreamClientID where

  parseJSON = gparseJson

instance JSONSchema UpstreamClientID where

  schema Proxy = schema (Proxy :: Proxy Text)

instance MessagePack UpstreamClientID where

  toObject (UpstreamClientID c) = toObject $ U.toText c

  fromObject x =
    fromObject x >>= \y ->
      case UpstreamClientID <$> U.fromText y of
        Nothing -> fail "Couldn't parse UpstreamClientID"
        Just t -> return t
