{-# LANGUAGE RankNTypes #-}

{-|
Module      : LensMap.Core
Copyright   : (c) UChicago Argonne, 2019
License     : BSD3
Maintainer  : fre@freux.fr
-}
module LensMap.Core where

import Control.Lens
import Data.Generics.Product
import Data.Map as DM
import Protolude

newtype ScopedLens s a = ScopedLens {getScopedLens :: Lens' s a}

type LensMap s k a = Map k (ScopedLens s (Maybe a))

class HasLensMap s k a where

  lenses :: s -> LensMap s k a

instance (Ord k, Ord key, HasLensMap (k, v) key a) => HasLensMap (Map k v) key a where

  lenses m = DM.fromList . mconcat $ DM.toList m <&> \(k, v) -> go k (lenses (k, v))
    where
      go
        :: (Ord k)
        => k
        -> Map key (ScopedLens (k, v) (Maybe a))
        -> [(key, ScopedLens (Map k v) (Maybe a))]
      go k lensMap =
        DM.toList lensMap <&> \(sensorKey, scopedLens) ->
          (sensorKey, lensAugmenter k scopedLens)
      lensAugmenter
        :: (Ord k)
        => k
        -> ScopedLens (k, v) (Maybe b)
        -> ScopedLens (Map k v) (Maybe b)
      lensAugmenter k = addMaybePath $ lens getter setter
        where
          getter lmap = DM.lookup k lmap <&> (k,)
          setter lmap (Just (_, value)) = DM.insert k value lmap

addPath :: Lens'  s' s -> ScopedLens s a -> ScopedLens  s' a
addPath l (ScopedLens sl) = ScopedLens (l . sl)

addMaybePath :: Lens'  s' (Maybe s) -> ScopedLens s a -> ScopedLens  s' a
addMaybePath l (ScopedLens sl) = undefined
