{-# LANGUAGE InstanceSigs #-}
{-# LANGUAGE RankNTypes #-}

-- |
-- Module      : LensMap.Core
-- Copyright   : (c) UChicago Argonne, 2019
-- License     : BSD3
-- Maintainer  : fre@freux.fr
module LensMap.Core
  ( ScopedLens (..),
    LensMap,
    HasLensMap (..),
    addPath,
  )
where

import Control.Lens
import Data.Map as DM
import Data.Maybe (fromJust)
import qualified LMap.Map as LM
import Protolude

newtype ScopedLens s a = ScopedLens {getScopedLens :: Lens' s a}

type LensMap s k a = Map k (ScopedLens s a)

class HasLensMap s k a where
  lenses :: s -> LensMap s k a

instance (Ord k, Ord key, HasLensMap (k, v) key a) => HasLensMap (Map k v) key a where
  lenses s = DM.fromList . mconcat $ DM.toList s <&> \(k, v) -> go k (lenses (k, v))
    where
      go ::
        forall key k v a.
        (Ord k) =>
        k ->
        Map key (ScopedLens (k, v) a) ->
        [(key, ScopedLens (Map k v) a)]
      go k lensMap =
        DM.toList lensMap <&> \(sensorKey, scopedLens) ->
          (sensorKey, augmentedLens k scopedLens)
      augmentedLens ::
        forall k v a.
        (Ord k) =>
        k ->
        ScopedLens (k, v) a ->
        ScopedLens (Map k v) a
      augmentedLens k = addPath $ lens getter setter
        where
          getter m = fromJust $ DM.lookup k m <&> (k,)
          setter m (_, value) = DM.insert k value m

instance (Ord k, Ord key, HasLensMap (k, v) key a) => HasLensMap (LM.Map k v) key a where
  lenses s = DM.fromList . mconcat $ LM.toList s <&> \(k, v) -> go k (lenses (k, v))
    where
      go ::
        forall key k v a.
        (Ord k) =>
        k ->
        Map key (ScopedLens (k, v) a) ->
        [(key, ScopedLens (LM.Map k v) a)]
      go k lensMap =
        DM.toList lensMap <&> \(sensorKey, scopedLens) ->
          (sensorKey, augmentedLens k scopedLens)
      augmentedLens ::
        forall k v a.
        Ord k =>
        k ->
        ScopedLens (k, v) a ->
        ScopedLens (LM.Map k v) a
      augmentedLens k = addPath $ lens getter setter
        where
          getter m = fromJust $ LM.lookup k m <&> (k,)
          setter m (_, value) = LM.insert k value m

addPath :: Lens' s' s -> ScopedLens s a -> ScopedLens s' a
addPath l (ScopedLens sl) = ScopedLens (l . sl)
