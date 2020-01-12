{-# OPTIONS_GHC -fno-warn-missing-signatures #-}
{-# OPTIONS_GHC -fno-warn-orphans #-}

-- |
-- Module      : main
-- Copyright   : (c) 2019, UChicago Argonne, LLC.
-- License     : BSD3
-- Maintainer  : fre@freux.fr
module Shared
  (
  )
where

import Data.List.NonEmpty
import Data.MessagePack
import FFI.TypeUncurry.Msgpack
import Foreign.C
import HBandit.Class
import HBandit.Exp3
import Protolude
import Refined hiding (NonEmpty)
import System.Random
import Prelude (fail)

-- | All FFI exported names in this module must have this opaque type
-- , must be followed by "Export", and must not use reserved symbols
-- like "stdout" or "stdin".
type Ex = CString -> IO CString

-----------------------------  Exp3 -------------------------------------------

foreign export ccall initExp3Export :: Ex

foreign export ccall stepExp3Export :: Ex

initExp3Export = exportIO initExp3

stepExp3Export = exportIO stepExp3

initExp3 :: Int -> IO (Exp3 Int, Int)
initExp3 hyper = do
  g <- liftIO getStdGen
  let (b, a, g') = HBandit.Class.init g (Arms (nub $ fromList [1 .. hyper]))
  liftIO $ setStdGen g'
  return (b, a)

stepExp3 :: Exp3 Int -> Double -> IO (Exp3 Int, Int)
stepExp3 b l =
  refine l & \case
    Left _ -> fail "loss not in [0,1]"
    Right loss -> do
      g <- liftIO getStdGen
      let ((a, g'), b') = runState (HBandit.Class.step g loss) b
      liftIO $ setStdGen g'
      return (b', a)

deriving instance MessagePack (Arms Int)

deriving instance MessagePack (Exp3 Int)

deriving instance MessagePack (Weight Int)

deriving instance MessagePack Probability

deriving instance MessagePack CumulativeLoss

instance (MessagePack a) => MessagePack (NonEmpty a) where

  toObject = toObject . Protolude.toList

  fromObject x =
    fromObject x >>= \y ->
      case nonEmpty y of
        Nothing -> fail "NonEmpty error in msgpack message"
        Just t -> return t
