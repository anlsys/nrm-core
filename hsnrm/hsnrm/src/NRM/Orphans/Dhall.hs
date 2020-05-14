{-# LANGUAGE ScopedTypeVariables #-}
{-# OPTIONS_GHC -fno-warn-orphans #-}

-- |
-- Module      : NRM.Orphans.ExitCode
-- Copyright   : (c) UChicago Argonne, 2019
-- License     : BSD3
-- Maintainer  : fre@freux.fr
module NRM.Orphans.Dhall
  (
  )
where

import Dhall
import Protolude

instance FromDhall Int where
  autoWith _ = fmap fromInteger integer
