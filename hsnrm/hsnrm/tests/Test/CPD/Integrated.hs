{-# OPTIONS_GHC -fno-warn-missing-export-lists #-}
{-# OPTIONS_GHC -fno-warn-monomorphism-restriction #-}
{-# OPTIONS_GHC -fno-warn-orphans #-}

module Test.CPD.Integrated where

import CPD.Integrated
import NRM.Types.Units
import Protolude hiding (one, zero)
import Test.Tasty.HUnit

unit_trapezoidArea :: IO ()
unit_trapezoidArea = do
  trapezoidArea (1, 1) (1, 1) @?= 0
  trapezoidArea (0, 1) (0.5, 1) @?= 0.5
  trapezoidArea (0, 1) (1, 1) @?= 1
  trapezoidArea (0, 0) (1, 1) @?= 0.5
  trapezoidArea (0, 0) (2, 1) @?= 1
  trapezoidArea (0, 2) (2, 2) @?= 4
  trapezoidArea (0, 2) (2, 3) @?= 5
  trapezoidArea (0, 3) (2, 2) @?= 5
  trapezoidArea (0, 1) (1, 0) @?= 0.5
  trapezoidArea (1, 0) (2, 1) @?= 0.5
  trapezoidArea (2, 1) (10, 1) @?= 8

unit_measureValue :: IO ()
unit_measureValue = do
  feed Never [(0, 1)] @?= Discarded
  Running
    ( M
        { firstTime = 2,
          lastTime = 2,
          lastValue = 1,
          area = 0
        }
    )
    @=? feed Never [(1, 1), (2, 1)]
  Running
    ( M
        { firstTime = 1,
          lastTime = 1,
          lastValue = 1,
          area = 0
        }
    )
    @=? feed Never [(0, 1), (1, 1)]
  Done
    ( M
        { firstTime = 1,
          lastTime = 11,
          lastValue = 1,
          area = 10
        }
    )
    @=? feed Never [(0, 0), (1, 1), (2, 1), (11, 1)]
  --   |
  --   |
  -- 1 |
  --   |\
  -- 0 L_\__________________
  --   0 1 2 3 4 5 6 7 8 9 10
  Done 0.5 @=? feedAvg Never [(0, 0), (1, 1), (2, 0), (11, 0)]
  --   |
  --   |
  -- 1 |    ________________
  --   |\  /
  -- 0 L_\/_________________
  --   0 1 2 3 4 5 6 7 8 9 10
  Running 8 @=? feedAvg Never [(0, 0), (1, 1), (2, 0), (3, 1), (10, 1)]
  Done 9 @=? feedAvg Never [(0, 0), (1, 1), (2, 0), (3, 1), (11, 1)]
  Done 10 @=? feedAvg Never [(0, 0), (1, 1), (11, 1)]
  Done
    ( M
        { firstTime = 0,
          lastTime = 2,
          lastValue = 2,
          area = arbitrary + 1.50
        }
    )
    @=? feed
      ( Done $
          M
            { firstTime = 0,
              lastTime = 1,
              lastValue = 1,
              area = arbitrary
            }
      )
      [(2, 2)]
  where
    feed :: MeasurementState M -> [(Time, Double)] -> MeasurementState M
    feed initialState ts = execState (for_ ts (modify . measure)) initialState
    feedAvg i ts = area <$> feed i ts
    measure = measureValue $ IntegratorMeta {tLast = 0, minimumWaitInterval = 0, minimumControlInterval = 10}
    arbitrary = 6543
