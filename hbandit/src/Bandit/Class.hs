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
    ContextualBandit (..),

    -- * Hyperparameters

    -- | These typeclass-based indirection layers help avoid unserializable
    -- hyperparameters.
    ExpertRepresentation (..),
    Rate (..),
  )
where

import Bandit.Types
import Data.Coerce
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

-- | ExpertRepresentation er s a is a representation that can be casted
-- into a distribution over actions.
--
-- @toExpert er@ returns the expert encoded as a conditional
-- distribution over actions.
class ExpertRepresentation er s a | er -> s, er -> a where
  toExpert :: er -> (s -> NonEmpty (ZeroOne Double, a))

instance ExpertRepresentation (ObliviousRep a) () a where
  toExpert (ObliviousRep l) () = l

-- | Rate r is a learning rate.
--
-- @toRate r@ returns the rate schedule.
class Rate r where
  toRate :: r -> Int -> Double

instance Rate FixedRate where
  toRate = const . coerce

instance Rate InverseSqrtRate where
  toRate x t = coerce x / sqrt (fromIntegral t)
