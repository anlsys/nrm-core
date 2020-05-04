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
    ExpertRepresentation (..),
    ContextualBandit (..),

    -- * Discrete Multi-Armed-Bandits
    Arms (..),
    ParameterFreeMAB (..),
  )
where

import Bandit.Types
import Protolude
import System.Random

-- | Bandit b hyper a l is the class for a bandit algorithm. We have the
-- following bandit process, (say, parametrized by hyperparameter
-- \(\eta \in \mathbb{H}\)) for \(t \in {1,\ldots,T}\):
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
class Bandit b hyper a l | b -> l, b -> hyper, b -> a where

  -- | Init hyper returns the initial state of the algorithm and the
  -- first action.
  init :: (RandomGen g) => g -> hyper -> (b, a, g)

  -- | @step loss@ iterates the bandit process one step forward.
  step :: (RandomGen g, MonadState b m) => g -> l -> m (a, g)

-- | ContextualBandit b hyper a l er is the class for a contextual bandit algorithm.
-- The same concepts as 'Bandit' apply, with the addition of:
--
-- * @er@ is an expert representation (see 'ExpertRepresentation')
class (ExpertRepresentation er s a) => ContextualBandit b hyper s a l er | b -> l, b -> hyper, b -> s, b -> a, b -> er where

  -- | Init hyper returns the initial state of the algorithm
  initCtx :: hyper -> b

  -- | @step loss@ iterates the bandit process one step forward.
  stepCtx :: (RandomGen g, MonadState b m, Ord a) => g -> l -> s -> m (a, g)

-- | ExpertRepresentation er s a is a distribution over
-- experts.
--
-- @represent er@ returns this distribution encoded as a conditional
-- distribution over actions.
class ExpertRepresentation er s a | er -> s, er -> a where
  represent :: er -> (s -> NonEmpty (ZeroOne Double, a))

-- | Arms a represents a set of possible actions.
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
