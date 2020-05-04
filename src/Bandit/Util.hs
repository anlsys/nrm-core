-- |
-- Module      : Bandit.Util
-- Copyright   : (c) 2019, UChicago Argonne, LLC.
-- License     : MIT
-- Maintainer  : fre@freux.fr
--
-- Utility functions for MAB algorithms.
module Bandit.Util
  ( sampleWL,
    normalize,
    unsafeNormalizePanic,
    normalizeDistribution,
    normalizedSum,
  )
where

import Bandit.Types
import Control.Monad.Random as MR
import Protolude
import Refined hiding (NonEmpty)
import Refined.Unsafe

-- | Samples from a weighted probability distribution.
-- The sum of weights must not be zero.
sampleWL :: RandomGen g => NonEmpty (ZeroOne Double, a) -> g -> (a, g)
sampleWL weights = runRand (MR.fromList $ toList (weights <&> \(r, x) -> (x, toRational (unrefine r))))

normalizeDistribution ::
  (Floating p, Ord p) =>
  NonEmpty (p, a) ->
  Maybe (NonEmpty (ZeroOne p, a))
normalizeDistribution xs = sequence $
  xs <&> \(p, a) ->
    case normalize p s of
      Nothing -> Nothing
      Just p' -> Just (p', a)
  where
    s = sum (fst <$> xs)

-- | @normalizedSum@ sums the `snd` from the tuple list using weights
-- in the `fst`. The result is normalized against the list of `fst`s.
normalizedSum ::
  (Ord a, Fractional a) =>
  [(ZeroOne a, ZeroOne a)] ->
  ZeroOne a
normalizedSum l =
  if sum (unrefine . fst <$> l) == 0
    then Bandit.Types.zero
    else
      unsafeRefine $
        sum
          ( l
              <&> \(w, v) -> unrefine w * unrefine v
          )
          / sum (unrefine . fst <$> l)

-- | @normalize x xm@ normalizes @x@ using @xm@. Returns @Nothing@
-- in case the resulting value is not contained in $[0,1]$ .
normalize :: (Floating a, Ord a) => a -> a -> Maybe (ZeroOne a)
normalize v m = refine (v / m) & \case
  Right n -> Just n
  Left _ -> Nothing

-- | @normalize x xm@ normalizes @x@ using @xm@. Returns @Nothing@
-- in case the resulting value is not contained in $[0,1]$ .
unsafeNormalizePanic :: (Floating a, Ord a) => a -> a -> ZeroOne a
unsafeNormalizePanic v m = refine (v / m) & \case
  Right n -> n
  Left _ -> panic "normalizePanic error."
