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

import Data.Random

class Bandit b f a l where

  init :: f a -> b

  action :: b -> a

  step :: (MonadRandom m) => b -> l -> m b
