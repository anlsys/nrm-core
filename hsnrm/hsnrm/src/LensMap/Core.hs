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
import qualified Data.Map as M
import Data.Maybe (fromJust)
import Protolude hiding (Map)

newtype ScopedLens s a = ScopedLens {getScopedLens :: Lens' s a}

type LensMap s k a = M.Map k (ScopedLens s a)

class HasLensMap s k a where
  lenses :: s -> LensMap s k a

instance (Ord k, Ord key, HasLensMap (k, v) key a) => HasLensMap (M.Map k v) key a where
  lenses s = M.fromList . mconcat $ M.toList s <&> \(k, v) -> go k (lenses (k, v))
    where
      go ::
        forall key k v a.
        (Ord k) =>
        k ->
        M.Map key (ScopedLens (k, v) a) ->
        [(key, ScopedLens (M.Map k v) a)]
      go k lensMap =
        M.toList lensMap <&> second (augmentedLens k)
      augmentedLens ::
        forall k v a.
        Ord k =>
        k ->
        ScopedLens (k, v) a ->
        ScopedLens (M.Map k v) a
      augmentedLens k = addPath $ lens getter setter
        where
          getter m = fromJust $ M.lookup k m <&> (k,)
          setter m (_, value) = M.insert k value m

addPath :: Lens' s' s -> ScopedLens s a -> ScopedLens s' a
addPath l (ScopedLens sl) = ScopedLens (l . sl)
