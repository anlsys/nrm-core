-- |
-- Module      : NRM.Version
-- Copyright   : (c) UChicago Argonne, 2019
-- License     : BSD3
-- Maintainer  : fre@freux.fr
module NRM.Version
  ( version,
  )
where

--import Distribution.VcsRevision.Git
--import Language.Haskell.TH.Syntax
import Protolude hiding (hash, lift)

version :: Text
version = "git"
