{-# LANGUAGE DerivingVia #-}

-- |
-- Module      : NRM.Types.MemBuffer
-- Copyright   : (c) UChicago Argonne, 2019
-- License     : BSD3
-- Maintainer  : swann@anl.gov
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
newtype MemBuffer = MemBuffer {getMemBuffer :: Refined (SizeLessThan 6) [Double]}
  deriving (Show, Eq, Ord, Generic, MessagePack, ToDhall, FromDhall)
  deriving (JSONSchema, ToJSON, FromJSON) via GenericJSON MemBuffer

-- | MemBuffer with one element
singleton :: Double -> MemBuffer
singleton a = [a]

-- | empty MemBuffer
empty :: MemBuffer
empty = []

-- | adding a data point to a MemBuffer, discarding the oldest point if there
-- are more than 5 points.
enqueue :: Double -> MemBuffer -> MemBuffer
enqueue x xs = fromList (x : toList xs)

-- | MemBuffer average
avgBuffer :: MemBuffer -> Double
avgBuffer (toList -> xs) = sum xs / fromIntegral (length xs)

instance IsList MemBuffer where

  type Item MemBuffer = Double

  fromList xs = MemBuffer . unsafeRefine $ take 5 xs

  toList (MemBuffer mb) = unrefine mb
