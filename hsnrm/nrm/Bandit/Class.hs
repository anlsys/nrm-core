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
import Protolude

class Bandit b f a l where

  init :: f a -> b

  getAction :: b -> Maybe a

  step :: (MonadRandom m) => b -> l -> m b
