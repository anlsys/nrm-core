{-# LANGUAGE DerivingVia #-}

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
  , parseContainerUUID
  , toText
  )
where

import Data.Aeson
import Data.JSON.Schema
import qualified Data.UUID as U (UUID (..), toText, fromText)
import Data.UUID.V1
import Generics.Generic.Aeson
import Protolude

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
