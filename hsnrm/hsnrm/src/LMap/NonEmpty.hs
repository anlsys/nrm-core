{-# LANGUAGE DerivingVia #-}
{-# LANGUAGE ScopedTypeVariables #-}

-- |
-- Module      : LMap.NonEmpty
-- Copyright   : (c) UChicago Argonne, 2019
-- License     : BSD3
-- Maintainer  : fre@freux.fr
--
-- Non-Empty Map based on Data.List.NonEmpty (with `Generic` instance).
module LMap.NonEmpty
  ( Map,
    LMap.NonEmpty.fromList,
    LMap.NonEmpty.toList,
    LMap.NonEmpty.map,
    lookup,
    insert,
    alter,
    delete,
    update,
    elems,
    keys,
    mapKV,
    toMap,
    toLMap,
  )
where

import Control.Lens
import Data.Data
import qualified Data.List.NonEmpty as NE (fromList, toList)
import qualified Data.Map as M
import Data.MessagePack
import qualified LMap.Map as LM
import NRM.Orphans.NonEmpty ()
import Protolude hiding (Map, toList)

-- | Association list with Data.Map interface and Control.Lens.At
-- instance. Useful to us because of the generic representation.
newtype Map a b = Map (NonEmpty (a, b))
  deriving (Show, Generic, Data, MessagePack, Functor, Foldable)
  deriving (Semigroup) via (NonEmpty (a, b))

toMap :: (Ord a) => Map a b -> M.Map a b
toMap = M.fromList . toList

toLMap :: Map a b -> LM.Map a b
toLMap = LM.fromList . toList

fromNEList :: NonEmpty (a, b) -> Map a b
fromNEList = Map

fromList :: [(a, b)] -> Maybe (Map a b)
fromList l = Map <$> nonEmpty l

unsafeFromList :: [(a, b)] -> Map a b
unsafeFromList l = Map $ NE.fromList l

toNEList :: Map a b -> NonEmpty (a, b)
toNEList (Map m) = m

toList :: Map a b -> [(a, b)]
toList (Map m) = NE.toList m

lookup :: Ord k => k -> Map k a -> Maybe a
lookup k (Map m) = M.lookup k (M.fromList $ NE.toList m)

insert :: Ord k => k -> a -> Map k a -> Map k a
insert k x (Map m) = M.insert k x (M.fromList $ NE.toList m) & M.toList & unsafeFromList

alter :: Ord k => (Maybe a -> Maybe a) -> k -> Map k a -> Map k a
alter f k m = m & toList & M.fromList & M.alter f k & M.toList & unsafeFromList

delete :: Ord k => k -> Map k a -> Maybe (Map k a)
delete k m = M.delete k (M.fromList $ toList m) & M.toList & fromList

update :: Ord k => (a -> Maybe a) -> k -> Map k a -> Map k a
update f k m = M.update f k (M.fromList $ toList m) & M.toList & unsafeFromList

elems :: Ord k => Map k a -> [a]
elems m = M.elems (M.fromList $ toList m)

keys :: Map k a -> NonEmpty k
keys (Map m) = fst <$> m

map :: (a -> b) -> Map k a -> Map k b
map f (Map m) = Map (m <&> second f)

mapKV :: ((a, b) -> (c, d)) -> Map a b -> Map c d
mapKV f m = m & LMap.NonEmpty.toNEList <&> f & LMap.NonEmpty.fromNEList

type instance Index (Map k a) = k

type instance IxValue (Map k a) = a
--instance (IsString a, JSONSchema b) => JSONSchema (Map a b) where
--schema _ = schema (Proxy :: Proxy (M.Map a b))
