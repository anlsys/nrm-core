{-# LANGUAGE DerivingVia #-}
{-# LANGUAGE TypeApplications #-}

-- |
-- Module      : NRM.Control
-- Copyright   : (c) UChicago Argonne, 2019
-- License     : BSD3
-- Maintainer  : fre@freux.fr
module NRM.Control
  ( control,
  )
where

import Control.Lens
import Data.Generics.Product
import NRM.Orphans.NonEmpty ()
import Protolude
import NRM.Types.Controller

control :: (MonadState Controller m) => Input -> m Decision
control (Reconfigure _time c) = do
  --field @"cpd" .= c
  {-field @"bandit" .= init [ ]-}
  return DoNothing
