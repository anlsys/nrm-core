{-# LANGUAGE DerivingVia #-}

-- |
-- Module      : NRM.Control
-- Copyright   : (c) UChicago Argonne, 2019
-- License     : BSD3
-- Maintainer  : fre@freux.fr
module NRM.Control
  ( control,
  )
where

--import Control.Lens
--import Data.Generics.Product
import NRM.Orphans.NonEmpty ()
import NRM.Types.Controller
import Protolude

control :: (MonadState Controller m) => Input -> m Decision
control (Reconfigure _time c) =
  --field @"cpd" .= c
  {-field @"bandit" .= init [ ]-}
  return DoNothing
