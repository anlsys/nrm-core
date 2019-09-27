{-# LANGUAGE DerivingVia #-}

{-|
Module      : NRM.Types.LMap
Copyright   : (c) UChicago Argonne, 2019
License     : BSD3
Maintainer  : fre@freux.fr
-}
module NRM.Types.LMap
  ( LMap
  , NRM.Types.LMap.fromList
  , NRM.Types.LMap.toList
  , NRM.Types.LMap.map
  , NRM.Types.LMap.empty
  , NRM.Types.LMap.null
  , lookup
  , insert
  , delete
  , update
  , elems
  , keys
  , mapKV
  )
where

import Control.Lens
import Data.Aeson
import Data.Data
import Data.JSON.Schema
import qualified Data.Map as DM
import Data.MessagePack
import NRM.Classes.Messaging
import Protolude hiding (toList)

-- | Sort of a hack around scaling limitations of generic-lens, This is
-- Data.Map with a stupid internal representation.  Useful for generic
-- programming where Map isn't supported yet.
newtype LMap a b = LMap [(a, b)]
  deriving (Show, Generic, Data, MessagePack, Functor, Foldable)
  deriving (JSONSchema, FromJSON, ToJSON) via GenericJSON (LMap a b)
  deriving (Semigroup, Monoid) via [(a, b)]

mapKV :: ((a, b) -> (c, d)) -> LMap a b -> LMap c d
mapKV f m = m & NRM.Types.LMap.toList <&> f & fromList

empty :: LMap a b
empty = LMap []

null :: LMap k a -> Bool
null (LMap []) = True
null _ = False

fromList :: [(a, b)] -> LMap a b
fromList = LMap

toList :: LMap a b -> [(a, b)]
toList (LMap m) = m

lookup :: Ord k => k -> LMap k a -> Maybe a
lookup k (LMap m) = DM.lookup k (DM.fromList m)

insert :: Ord k => k -> a -> LMap k a -> LMap k a
insert k x (LMap m) = DM.insert k x (DM.fromList m) & DM.toList & fromList

delete :: Ord k => k -> LMap k a -> LMap k a
delete k (LMap m) = DM.delete k (DM.fromList m) & DM.toList & fromList

update :: Ord k => (a -> Maybe a) -> k -> LMap k a -> LMap k a
update f k (LMap m) = DM.update f k (DM.fromList m) & DM.toList & fromList

elems :: Ord k => LMap k a -> [a]
elems (LMap m) = DM.elems (DM.fromList m)

keys :: Ord k => LMap k a -> [k]
keys (LMap m) = DM.keys (DM.fromList m)

map :: (Ord k) => (a -> b) -> LMap k a -> LMap k b
map f (LMap m) = DM.map f (DM.fromList m) & DM.toList & fromList

type instance Index (LMap k a) = k

type instance IxValue (LMap k a) = a

instance Ord k => Ixed (LMap k a) where

  ix k f m = case lookup k m of
    Just v -> f v <&> \v' -> insert k v' m
    Nothing -> pure m

instance Ord k => At (LMap k a) where

  at k f m =
    f mv <&> \case
      Nothing -> maybe m (const (delete k m)) mv
      Just v' -> insert k v' m
    where
      mv = lookup k m
