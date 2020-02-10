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

import Protolude hiding (empty)
import Refined
import Refined.Unsafe

-- | Memory buffer with 10 points.
type MemBuffer a = Refined (SizeLessThan 10) [a]

-- | MemBuffer with one element
singleton :: a -> MemBuffer a
singleton a = unsafeRefine [a]

-- | empty MemBuffer
empty :: MemBuffer a
empty = unsafeRefine []

-- | adding a data point to a MemBuffer, discarding the oldest point if there
-- are more than 10 points.
enqueue :: a -> MemBuffer a -> MemBuffer a
enqueue x xs = unsafeRefine $ take 9 (x : unrefine xs)

-- | adding a data point to a MemBuffer, discarding the oldest point if there
-- are more than 10 points.
avgBuffer :: (Floating a) => MemBuffer a -> a
avgBuffer (unrefine -> xs) = sum xs / fromIntegral (length xs)
