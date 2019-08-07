{-|
Module      : hsnrm
Copyright   : (c) 2019, UChicago Argonne, LLC.
License     : BSD3
Maintainer  : fre@freux.fr
-}
module Hnrm
  ( main
  )
where

import Prelude
import Nrm.Optparse

-- | The main client process
main :: IO ()
main = parseClientCli >>= print
