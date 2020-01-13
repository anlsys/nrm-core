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

exp3 :: Exportable (Exp3 Int) Int Int Double
exp3 =
  exportBandit
    (\count -> Right $ Arms (nub $ fromList [1 .. count]))
    refine

initExp3Export = exportIO (initExportable exp3)

stepExp3Export = exportIO (stepExportable exp3)

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

----- typeclass based exporting functions

data Exportable b hyper a loss
  = Exportable
      { initExportable :: hyper -> IO (b, a),
        stepExportable :: b -> loss -> IO (b, a)
      }

exportBandit ::
  (Bandit b hyper a loss) =>
  (hyper' -> Either e1 hyper) ->
  (loss' -> Either e2 loss) ->
  Exportable b hyper' a loss'
exportBandit hyperBuilder lossBuilder =
  Exportable
    { initExportable = \hyper' -> hyperBuilder hyper' & \case
        Left _ -> fail "hyperparameter refinement failed"
        Right hyper -> do
          g <- liftIO getStdGen
          let (b, a, g') = HBandit.Class.init g hyper
          liftIO $ setStdGen g'
          return (b, a),
      stepExportable = \b loss' ->
        lossBuilder loss' & \case
          Left _ -> fail "loss refinement failed"
          Right loss -> do
            g <- liftIO getStdGen
            let ((a, g'), b') = runState (HBandit.Class.step g loss) b
            liftIO $ setStdGen g'
            return (b', a)
    }
