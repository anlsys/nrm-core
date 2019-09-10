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

-- | Bandit b f a l is the class for a bandit alrogithm, where b is the bandit
-- state, f is the datatype describing the space of available actions (useful if
-- dynamically known) at runtime, a is a superset of this space
-- (useful if statically known), and l is the space of admissible losses.
class Bandit b f a l where

  -- | Initial bandit state construction
  init :: f a -> b

  -- | iterating one step through the bandit process
  step :: (MonadRandom m, MonadState b m) => l -> m a
