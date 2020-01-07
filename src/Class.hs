{-# LANGUAGE ScopedTypeVariables #-}

-- |
-- Module      : Bandit.Class
-- Copyright   : (c) 2019, UChicago Argonne, LLC.
-- License     : MIT
-- Maintainer  : fre@freux.fr
--
-- This module implements the common interface for instanciating
-- and interacting with Multi-Armed Bandit algoritms.
module Bandit.Class
  ( -- * Generalized Bandit
    Bandit (..),

    -- * Discrete Multi-Armed-Bandits
    Arms (..),
    ParameterFreeMAB (..),
    BwCRMAB (..),
  )
where

import Protolude
import System.Random

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
-- * @a@ is a superset of admissible actions \(\mathbb{A}\) (statically
-- known).
--
-- * @l@ is a superset of admissible losses \(\mathbb{L}\) (statically
-- known).
class Bandit b hyper a l | b -> l, b -> hyper where

  -- | Init hyper returns the initial state of the bandit algorithm and the
  -- first action.
  init :: (RandomGen g) => g -> hyper -> (b, a, g)

  -- | @step loss@ iterates the bandit process one step forward.
  step :: (RandomGen g, MonadState b m) => g -> l -> m (a, g)

newtype Arms a = Arms (NonEmpty a)
  deriving (Show, Generic)

-- | Hyper-Parameter-free MAB. In this context, \(\mathbb{L}\) is known
-- statically, and \(\mathbb{A}\) is specified by \(\mathbb{H}\)=@Arms a@,
-- which is the set of finite non-empty sets. We define the regret \(R_T\) as:
--
-- \[ R_T = \sum_{t=1}^{T} \ell_{a^t}^t - \text{min}_{a=1}^{K} \sum_{t=1}^{T}
-- \ell_{a}^t \]
class (Eq a, Bandit b (Arms a) a l) => ParameterFreeMAB b a l | b -> l where

  -- | @init as@ returns the initial state of the bandit algorithm, where @as@
  -- is a set of available actions.
  initPFMAB :: (RandomGen g) => g -> Arms a -> (b, a, g)
  initPFMAB = init

  -- | @step l@ iterates the bandit process one step forward by feeding loss
  -- value @l@.
  stepPFMAB :: (RandomGen g, MonadState b m) => g -> l -> m (a, g)
  stepPFMAB = step

-- | The BwCR bandit framework.
--
-- In this context, (\mathbb{L} \in \mathbb{[0,1]}^d\), \(\mathbb{A}\) is
-- specified by \(\mathbb{H}\)=@p l@, which specifies the set of arms,
-- objective function and constraint set.
--
-- We define two notions of regret: The objective-regret \(R_{\text{obj}}\),
-- and the constraint-regret \(R_{\text{cst}}\).
--
-- \[ R_{obj} = \] \[ R_{cst} = \]
class (Eq a, Bandit b (p l) a l) => BwCRMAB b a p l | b -> l, b -> p where

  -- | @init as@ returns the initial state of the bandit algorithm, where @as@
  -- is a set of available actions.
  initBwCR :: (RandomGen g) => g -> p l -> (b, a, g)
  initBwCR = init

  -- | @step l@ iterates the bandit process one step forward by feeding loss
  -- value @l@.
  stepBwCR :: (RandomGen g, MonadState b m) => g -> l -> m (a, g)
  stepBwCR = step
