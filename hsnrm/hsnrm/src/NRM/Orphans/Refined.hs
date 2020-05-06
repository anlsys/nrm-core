{-# LANGUAGE DerivingVia #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# OPTIONS_GHC -fno-warn-orphans #-}

-- |
-- Module      : NRM.Orphans.Refined
-- Copyright   : (c) UChicago Argonne, 2019
-- License     : BSD3
-- Maintainer  : fre@freux.fr
module NRM.Orphans.Refined
  (
  )
where

import Data.Functor.Contravariant (contramap)
import Data.JSON.Schema
import Data.MessagePack
import Dhall
import Protolude
import Refined
import Refined.Orphan.Aeson ()
import Refined.Unsafe

instance (JSONSchema a) => JSONSchema (Refined p a) where
  schema _ = schema (Proxy :: Proxy a)

instance (Predicate p a, MessagePack a) => MessagePack (Refined p a) where

  toObject = toObject . unrefine

  fromObject x =
    fromObject x >>= \y ->
      case refine y of
        Left e -> panic $ show e
        Right t -> return t

instance (Predicate p a, Interpret a) => Interpret (Refined p a) where
  autoWith = fmap unsafeRefine . autoWith

instance (Inject a) => Inject (Refined p a) where
  injectWith = fmap (contramap unrefine) Dhall.injectWith
