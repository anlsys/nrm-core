{-# LANGUAGE DerivingVia #-}

-- |
-- Module      : NRM.Types.MemBuffer
-- Copyright   : (c) UChicago Argonne, 2019
-- License     : BSD3
-- Maintainer  : fre@freux.fr
module NRM.Types.MemBuffer
  ( MemBuffer,
    empty,
    enqueue,
    singleton,
    avgBuffer,
  )
where

import Data.Aeson
import Data.MessagePack
import Dhall
import GHC.Exts
import NRM.Classes.Messaging
import NRM.Orphans.Refined ()
import Protolude hiding (empty, toList)
import Refined
import Refined.Unsafe

-- | Memory buffer with 5 points.
newtype MemBuffer a = MemBuffer {getMemBuffer :: Refined (SizeLessThan 6) [a]}
  deriving (Show, Eq, Ord, Generic, MessagePack, Inject)
  deriving (JSONSchema, ToJSON, FromJSON) via GenericJSON (MemBuffer a)

instance Interpret a => Interpret (MemBuffer a) where
  autoWith opts = fromList <$> Dhall.list (autoWith opts)

-- | MemBuffer with one element
singleton :: a -> MemBuffer a
singleton a = [a]

-- | empty MemBuffer
empty :: MemBuffer a
empty = []

-- | adding a data point to a MemBuffer, discarding the oldest point if there
-- are more than 5 points.
enqueue :: a -> MemBuffer a -> MemBuffer a
enqueue x xs = fromList (x : toList xs)

-- | MemBuffer average
avgBuffer :: (Floating a) => MemBuffer a -> a
avgBuffer (toList -> xs) = sum xs / fromIntegral (length xs)

instance IsList (MemBuffer a) where

  type Item (MemBuffer a) = a

  fromList xs = MemBuffer . unsafeRefine $ take 5 xs

  toList (MemBuffer mb) = unrefine mb
