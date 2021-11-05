-- |
-- Module      : NRM.Version
-- Copyright   : (c) UChicago Argonne, 2019
-- License     : BSD3
-- Maintainer  : swann@anl.gov
module NRM.Version
  ( version,
  )
where

--import Distribution.VcsRevision.Git
--import Language.Haskell.TH.Syntax
import Protolude hiding (hash, lift)

version :: Text
version = "git"
