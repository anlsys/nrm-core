{-# LANGUAGE ScopedTypeVariables #-}
{-# OPTIONS_GHC -fno-warn-orphans #-}

-- |
-- Module      : NRM.Orphans.NonEmpty
-- Copyright   : (c) UChicago Argonne, 2019
-- License     : BSD3
-- Maintainer  : swann@anl.gov
module NRM.Orphans.NonEmpty
  (
  )
where

import Data.Functor.Contravariant (contramap)
import Data.JSON.Schema
import Data.Maybe
import Data.MessagePack
import Dhall
import Protolude

instance (JSONSchema a) => JSONSchema (NonEmpty a) where
  schema _ = schema (Proxy :: Proxy [a])

instance (MessagePack a) => MessagePack (NonEmpty a) where

  toObject = toObject . toList

  fromObject x =
    fromObject x >>= \y ->
      case nonEmpty y of
        Nothing -> panic "NonEmpty error in msgpack message"
        Just t -> return t

data NEDhall a
  = NEDhall
      { neHead :: a,
        neTail :: [a]
      }
  deriving (Generic)

deriving instance (FromDhall a) => FromDhall (NEDhall a)

deriving instance (ToDhall a) => ToDhall (NEDhall a)

nonEmptyDhallToNonEmptyList :: NEDhall a -> NonEmpty a
nonEmptyDhallToNonEmptyList NEDhall {..} = neHead :| neTail

nonEmptyListToNonEmptyDhall :: NonEmpty a -> NEDhall a
nonEmptyListToNonEmptyDhall (neHead :| neTail) = NEDhall {..}

instance (Interpret a) => FromDhall (NonEmpty a) where
  autoWith = fmap nonEmptyDhallToNonEmptyList . autoWith

instance (Inject a) => ToDhall (NonEmpty a) where
  injectWith = fmap (contramap nonEmptyListToNonEmptyDhall) Dhall.injectWith
