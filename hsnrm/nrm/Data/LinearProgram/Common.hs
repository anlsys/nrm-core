{-# OPTIONS_GHC -fno-warn-orphans #-}

-- | Contains sufficient tools to represent linear programming problems in Haskell.  In the future, if linkings to other
-- linear programming libraries are made, this will be common to them all.
module Data.LinearProgram.Common
  ( module Data.LinearProgram.Spec,
    module Algebra.Classes,
    module Data.LinearProgram.Types,
  )
where

import Algebra.Classes
import Data.LinearProgram.Spec
import Data.LinearProgram.Types
import Data.Map (assocs, elems, foldrWithKey, keys)
import GHC.Exts (build)
import Protolude

{-# RULES
"assocs" assocs = \m -> build (\c n -> foldrWithKey (curry c) n m)
"elems" elems = \m -> build (\c n -> foldrWithKey (const c) n m)
"keys" keys = \m -> build (\c n -> foldrWithKey (\k _ -> c k) n m)
  #-}
