{-# LANGUAGE ScopedTypeVariables #-}

{-|
Module      : Nrm.Classes.Topology
Copyright   : (c) UChicago Argonne, 2019
License     : BSD3
Maintainer  : fre@freux.fr
-}
module Nrm.Classes.Topology
  ( IdFromString (..), ToHwlocType (..)
  )
where

import Protolude
import Prelude (String)

class IdFromString a where

  idFromString :: String -> Maybe a

-- | translating to hwloc XML "type" field.
class ToHwlocType a where

  getType :: Proxy a -> Text
