{-# LANGUAGE DerivingVia #-}

{-|
Module      : LMap.Map
Copyright   : (c) UChicago Argonne, 2019
License     : BSD3
Maintainer  : fre@freux.fr
-}
module LMap.Map
  ( Map
  , LMap.Map.fromList
  , LMap.Map.toList
  , LMap.Map.map
  , LMap.Map.empty
  , LMap.Map.null
  , lookup
  , insert
  , alter
  , delete
  , update
  , elems
  , keys
  , mapKV
  )
where

import Control.Lens
import Data.Data
import qualified Data.Map as DM hiding (Map)
import Data.MessagePack
import Protolude hiding (Map, toList)

-- | Association list with Data.Map interface and Control.Lens.At
-- instance. Useful to us because of the generic representation.
newtype Map a b = Map [(a, b)]
  deriving (Show, Generic, Data, MessagePack, Functor, Foldable)
  deriving (Semigroup, Monoid) via [(a, b)]

mapKV :: ((a, b) -> (c, d)) -> Map a b -> Map c d
mapKV f m = m & LMap.Map.toList <&> f & fromList

empty :: Map a b
empty = Map []

null :: Map k a -> Bool
null (Map []) = True
null _ = False

fromList :: [(a, b)] -> Map a b
fromList = Map

toList :: Map a b -> [(a, b)]
toList (Map m) = m

lookup :: Ord k => k -> Map k a -> Maybe a
lookup k (Map m) = DM.lookup k (DM.fromList m)

insert :: Ord k => k -> a -> Map k a -> Map k a
insert k x (Map m) = DM.insert k x (DM.fromList m) & DM.toList & fromList

alter :: Ord k => (Maybe a -> Maybe a) -> k -> Map k a -> Map k a
alter f k m = m & toList & DM.fromList & DM.alter f k & DM.toList & fromList

delete :: Ord k => k -> Map k a -> Map k a
delete k (Map m) = DM.delete k (DM.fromList m) & DM.toList & fromList

update :: Ord k => (a -> Maybe a) -> k -> Map k a -> Map k a
update f k (Map m) = DM.update f k (DM.fromList m) & DM.toList & fromList

elems :: Ord k => Map k a -> [a]
elems (Map m) = DM.elems (DM.fromList m)

keys :: Ord k => Map k a -> [k]
keys (Map m) = DM.keys (DM.fromList m)

map :: (Ord k) => (a -> b) -> Map k a -> Map k b
map f (Map m) = DM.map f (DM.fromList m) & DM.toList & fromList

type instance Index (Map k a) = k

type instance IxValue (Map k a) = a

instance Ord k => Ixed (Map k a) where

  ix k f m = case lookup k m of
    Just v -> f v <&> \v' -> insert k v' m
    Nothing -> pure m

instance Ord k => At (Map k a) where

  at k f m =
    f mv <&> \case
      Nothing -> maybe m (const (delete k m)) mv
      Just v' -> insert k v' m
    where
      mv = lookup k m
