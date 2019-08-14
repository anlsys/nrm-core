{-|
Module      : Nrm.Types.Container
Copyright   : (c) UChicago Argonne, 2019
License     : BSD3
Maintainer  : fre@freux.fr
-}
module Nrm.Types.Container
  ( {-Container (..)-}
    ContainerUUID (..)
  , nextContainerUUID
  , parseContainerUUID
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

{-data Container = Container {downstreamClients :: [D]}-}
data ContainerUUID = ContainerUUID U.UUID | Name Text
  deriving (Show, Eq, Ord, Generic)

nextContainerUUID :: IO (Maybe ContainerUUID)
nextContainerUUID = fmap ContainerUUID <$> nextUUID

parseContainerUUID :: Text -> ContainerUUID
parseContainerUUID t = case U.fromText t of
  Just x -> ContainerUUID x
  Nothing -> Name t

toText :: ContainerUUID -> Text
toText (ContainerUUID u) = U.toText u
toText (Name n) = n

instance ToJSON ContainerUUID where

  toJSON = gtoJson

instance FromJSON ContainerUUID where

  parseJSON = gparseJson

instance JSONSchema ContainerUUID where

  schema Proxy = schema (Proxy :: Proxy Text)

instance MessagePack ContainerUUID where

  toObject (ContainerUUID c) = toObject $ U.toText c
  toObject (Name c) = toObject c

  fromObject x = fromObject x <&> parseContainerUUID
