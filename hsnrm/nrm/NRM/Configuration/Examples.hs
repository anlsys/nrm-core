{-# OPTIONS_GHC -fno-warn-orphans #-}

-- |
-- Module      : NRM.Configuration.Examples
-- Copyright   : (c) 2019, UChicago Argonne, LLC.
-- License     : BSD3
-- Maintainer  : fre@freux.fr
module NRM.Configuration.Examples
  (
  )
where

import Data.Default
import qualified Data.Map as M
import NRM.Classes.Examples
import NRM.Types.Configuration

instance Examples Cfg where
  examples = M.fromList [("default", def)]
