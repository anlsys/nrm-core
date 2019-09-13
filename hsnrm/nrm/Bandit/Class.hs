{-|
Module      : Bandit.Class
Copyright   : (c) 2019, UChicago Argonne, LLC.
License     : MIT
Maintainer  : fre@freux.fr
-}
module Bandit.Class
  ( -- * Generalized Bandit
    Bandit (..)
  , -- * Discrete Multi-Armed-Bandits
    ParameterFreeMAB (..)
  )
where

import Data.Random
import Protolude

-- | Bandit b hyper f a l is the class for a bandit algorithm. This is mostly
-- here to help structure the library itself.  We have the following bandit
-- process, (say, parametrized by hyperparameter \(\eta \in \mathbb{H}\)) for 
-- \(t \in {1,\ldots,T}\):
--
-- - Nature selects losses \(\ell_{a}^t \in \mathbb{L} \forall a \in \mathbb{A}\)
--
-- -  The algorithm chooses action \(a^t \in \mathbb{A}\)
--
-- -  The algorithm observes loss \(\ell_{a^t}^t \in \mathbb{L}\)
--
-- Class parameters are organized in the following way:
--
-- * @b@ is the bandit state datatype.
--
-- * @hyper@ is the space \(\mathbb{H}\) of hyperparameter(s) for the
-- algorithm.
--
-- * @fA a@ describes the space of admissible actions \(\mathbb{A}\)
-- (configuration at runtime).
--
-- * @a@ is a superset of admissible actions \(\mathbb{A}\) (statically
-- known).
--
-- * @fL l@ describes the space of admissible losses \(\mathbb{L}\)
-- (configuration at runtime).
--
-- * @l@ is a superset of admissible losses \(\mathbb{L}\) (statically
-- known).
--
class Bandit b hyper fA fL a l | b -> l, b -> fA, b -> fL, b -> hyper where

  -- | Init hyper fa fl returns the initial state of the bandit algorithm.
  init :: hyper -> fA a -> fL l -> b

  -- | @step loss@ iterates the bandit process one step forward.
  step :: (MonadRandom m, MonadState b m) => l -> m a

-- | Hyper-Parameter-free MAB with statically known loss range. In this
-- context, \(\mathbb{L}= [a,b]\), \(\mathbb{A}\) is a finite set (known at
-- configuration time) and \(\mathbb{H}=\emptyset\) is empty. We define the
-- regret \(R_T\) as :
--
-- \[ R_T = \sum_{t=1}^{T} \ell_{a^t}^t - \text{min}_{a=1}^{K} \sum_{t=1}^{T}
-- \ell_{a}^t \]
--
class (Eq a, Bandit b () Set Proxy a l) => ParameterFreeMAB b a l | b -> l where

  -- | @init as@ returns the initial state of the bandit algorithm, where @as@
  -- is a set of available actions.
  initMAB :: Set a -> b
  initMAB as = init () as Proxy

  -- | @step l@ iterates the bandit process one step forward by feeding loss
  -- value @l@.
  stepMAB :: (MonadRandom m, MonadState b m) => l -> m a
  stepMAB = step
