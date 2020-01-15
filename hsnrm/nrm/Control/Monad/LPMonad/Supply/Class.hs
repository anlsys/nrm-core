{-# LANGUAGE UndecidableInstances #-}
{-# OPTIONS_GHC -fno-warn-deprecations #-}

module Control.Monad.LPMonad.Supply.Class
  ( MonadSupply (..),
  )
where

import Control.Monad
import Control.Monad.Cont
import Control.Monad.Error
import Control.Monad.Reader
import qualified Control.Monad.State.Lazy as SL
import Control.Monad.State.Strict
import qualified Control.Monad.Writer.Lazy as WL
import qualified Control.Monad.Writer.Strict as WS
import Data.Monoid
import Prelude

-- | A class implemented by monads that can supply values of type @s@.  Minimal
-- implementation: 'supplyNew' or 'supplyN'.
class Monad m => MonadSupply s m | m -> s where

  -- | Supply a new value of type @s@.
  supplyNew :: m s

  -- | Supply @n@ values of type @s@.
  supplyN :: Int -> m [s]

  supplyNew = head <$> supplyN 1

  supplyN n = replicateM n supplyNew

instance MonadSupply x m => MonadSupply x (StateT s m) where

  supplyNew = lift supplyNew

  supplyN = lift . supplyN

instance MonadSupply x m => MonadSupply x (ReaderT r m) where

  supplyNew = lift supplyNew

  supplyN = lift . supplyN

instance (Error e, MonadSupply x m) => MonadSupply x (ErrorT e m) where

  supplyNew = lift supplyNew

  supplyN = lift . supplyN

instance (MonadSupply x m, Monoid w) => MonadSupply x (WL.WriterT w m) where

  supplyNew = lift supplyNew

  supplyN = lift . supplyN

instance (MonadSupply x m, Monoid w) => MonadSupply x (WS.WriterT w m) where

  supplyNew = lift supplyNew

  supplyN = lift . supplyN

instance MonadSupply x m => MonadSupply x (ContT r m) where

  supplyNew = lift supplyNew

  supplyN = lift . supplyN

instance MonadSupply x m => MonadSupply x (SL.StateT s m) where

  supplyNew = lift supplyNew

  supplyN = lift . supplyN
