{-# LANGUAGE TemplateHaskell #-}

{-|
Module      : Nrm.Version
Copyright   : (c) UChicago Argonne, 2019
License     : BSD3
Maintainer  : fre@freux.fr
-}
module Nrm.Version
  ( version
  )
where

import Distribution.VcsRevision.Git
import Language.Haskell.TH.Syntax
import Protolude hiding (hash, lift)

version :: Text
version =
  $( do
       v <- qRunIO getRevision
       lift $ case v of
         Nothing -> "<none>"
         Just (hash, True) -> hash ++ " (with local modifications)"
         Just (hash, False) -> hash
   )
