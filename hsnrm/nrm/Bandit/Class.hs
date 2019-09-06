{-|
Module      : Bandit.Class
Copyright   : (c) 2019, UChicago Argonne, LLC.
License     : MIT
Maintainer  : fre@freux.fr
-}
module Bandit.Class
  ( Bandit (..)
  )
where

import Protolude
import Control.Monad.Random

class Bandit b a reward where

  initialBandit :: Int -> b

  step :: (MonadRandom m) => b -> m (b, a)
