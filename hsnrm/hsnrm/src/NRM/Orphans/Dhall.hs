{-# LANGUAGE ScopedTypeVariables #-}
{-# OPTIONS_GHC -fno-warn-orphans #-}

-- |
-- Module      : NRM.Orphans.ExitCode
-- Copyright   : (c) UChicago Argonne, 2019
-- License     : BSD3
-- Maintainer  : fre@freux.fr
module NRM.Orphans.Dhall
  (
  )
where

import Data.Functor.Contravariant (contramap)
import Data.Map as M
import Dhall
import Protolude

instance Interpret Int where
  autoWith _ = fmap fromInteger integer

instance
  (Ord a, Interpret a, Interpret b) =>
  Interpret (Map a b)
  where
  autoWith _ =
    fmap
      M.fromList
      ( Dhall.list
          (pair (auto :: Dhall.Type a) (auto :: Dhall.Type b))
      )

instance
  (Inject a, Inject b) =>
  Dhall.Inject (Map a b)
  where
  injectWith = fmap (contramap M.toList) Dhall.injectWith
