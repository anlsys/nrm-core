{-# OPTIONS_GHC -fno-warn-orphans  #-}

{-|
Module      : Nrm.Types.Container
Copyright   : (c) UChicago Argonne, 2019
License     : BSD3
Maintainer  : fre@freux.fr
-}
module Nrm.Types.Container
  ( ContainerUUID (..)
  , nextContainerUUID
  )
where

import Data.Aeson
import Data.JSON.Schema
import Data.UUID
import Data.UUID.V1
import Generics.Generic.Aeson
import Protolude

data ContainerUUID = ContainerUUID UUID | Name Text
  deriving (Eq, Ord, Generic)

instance ToJSON ContainerUUID where

  toJSON = gtoJson

instance FromJSON ContainerUUID where

  parseJSON = gparseJson

instance JSONSchema ContainerUUID where

  schema = gSchema

instance JSONSchema UUID where

  schema _ = schema (Proxy :: Proxy Text)

nextContainerUUID :: IO (Maybe ContainerUUID)
nextContainerUUID = fmap ContainerUUID <$> nextUUID
