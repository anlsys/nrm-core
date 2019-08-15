{-|
Module      : Nrm.Types.DownstreamClient
Copyright   : (c) UChicago Argonne, 2019
License     : BSD3
Maintainer  : fre@freux.fr
-}
module Nrm.Types.DownstreamClient
  ( DownstreamID (..)
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
import Nrm.Types.Process as P
import Protolude
import Prelude (fail)

newtype DownstreamID = DownstreamID P.ProcessID
  deriving (Eq, Ord, Show, Generic)

parseDownstreamID :: Text -> Maybe DownstreamID
parseDownstreamID = fmap DownstreamID <$> readMaybe . toS

toText :: DownstreamID -> Text
toText (DownstreamID u) = show u

instance ToJSON DownstreamID where

  toJSON = gtoJson

instance FromJSON DownstreamID where

  parseJSON = gparseJson

instance JSONSchema DownstreamID where

  schema Proxy = schema (Proxy :: Proxy Text)

deriving instance MessagePack DownstreamID
