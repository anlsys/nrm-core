{-# LANGUAGE ScopedTypeVariables #-}

{-# OPTIONS_GHC -fno-warn-orphans #-}
{-|
Module      : NRM.Orphans.NonEmpty
Copyright   : (c) UChicago Argonne, 2019
License     : BSD3
Maintainer  : fre@freux.fr
-}
module NRM.Orphans.NonEmpty
  (
  )
where

import Data.JSON.Schema
import Data.Maybe
import Data.MessagePack
import Dhall
import Protolude
import Data.Functor.Contravariant (contramap)
import Prelude (fail)

instance (JSONSchema a) => JSONSchema (NonEmpty a) where

  schema _ = schema (Proxy :: Proxy ([a]))

instance (MessagePack a) => MessagePack (NonEmpty a) where

  toObject = toObject . toList

  fromObject x =
    fromObject x >>= \y ->
      case nonEmpty y of
        Nothing -> fail "NonEmpty error in msgpack message"
        Just t -> return t

instance (Interpret a) => Interpret (NonEmpty a) where

  autoWith = fmap (fromJust . nonEmpty) . autoWith

instance (Inject a) => Inject (NonEmpty a) where

  injectWith = fmap (contramap toList) Dhall.injectWith
