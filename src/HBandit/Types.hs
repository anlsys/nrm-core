{-# OPTIONS_GHC -fno-warn-incomplete-patterns #-}
{-# OPTIONS_GHC -fno-warn-missing-local-signatures #-}

-- |
-- Module      : HBandit.Util
-- Copyright   : (c) 2019, UChicago Argonne, LLC.
-- License     : MIT
-- Maintainer  : fre@freux.fr
--
-- Utility functions for MAB algorithms.
module HBandit.Types
  ( ZeroOne,
    HBandit.Types.zero,
    HBandit.Types.one,
    normalize,
    normalizedSum,
  )
where

import Protolude
import Refined
import Refined.Unsafe

-- | Type alias for a \(\mathbb{L}=[0,1]\) refined value.
type ZeroOne l = (Refined (FromTo 0 1) l)

-- | 0
zero :: (Ord a, Num a) => ZeroOne a
zero = unsafeRefine 0

-- | 1
one :: (Ord a, Num a) => ZeroOne a
one = unsafeRefine 1

-- | @normalizedSum@ sums the `snd` from the tuple list using weights
-- in the `fst`. The result is normalized against the list of `fst`s.
normalizedSum ::
  (Ord a, Fractional a) =>
  [(ZeroOne a, ZeroOne a)] ->
  ZeroOne a
normalizedSum l =
  if sum (unrefine . fst <$> l) == 0
    then HBandit.Types.zero
    else
      unsafeRefine $
        sum
          ( l
              <&> \(w, v) -> unrefine w * unrefine v
          )
          / sum (unrefine . fst <$> l)

-- | @normalize x xm@ normalizes @x@ using @xm@. Returns @Nothing@
-- in case the resulting value is not contained in $[0,1]$ .
normalize :: Double -> Double -> Maybe (ZeroOne Double)
normalize v m = refine (v / m) & \case
  Right n -> Just n
  Left _ -> Nothing
