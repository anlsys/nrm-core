module Bandit.UtilTest where

import Bandit.Types
import Bandit.TypesTest
import Bandit.Util
import Protolude hiding (one, zero)
import Refined
import Refined.Orphan.QuickCheck
import Refined.Unsafe
import System.Random
import Test.Tasty.HUnit
import Test.Tasty.QuickCheck

unit_sampleWL :: IO ()
unit_sampleWL = do
  let g = mkStdGen 3
  fst (sampleWL [(one, 4)] g) @?= 4

prop_normalizedSum :: ZeroOne Double -> ZeroOne Double -> Bool
prop_normalizedSum xr@(unrefine -> x) yr@(unrefine -> y) =
  normalizedSum [(one, xr), (unsafeRefine 0.5, yr)]
    == unsafeRefine ((x + 0.5 * y) / 1.5)

unit_normalizedSum :: IO ()
unit_normalizedSum =
  normalizedSum [(one, one), (one, zero)]
    @?= reallyUnsafeRefine 0.5

prop_normalize :: Double -> Double -> Bool
prop_normalize (abs -> a) (abs -> b) = a & \case
  0 -> True
  a' -> normalize a (a + b) & \case
    Nothing -> False
    Just (unrefine -> x) -> x == a / (a + b)
