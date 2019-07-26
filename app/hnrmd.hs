{-# LANGUAGE NoImplicitPrelude #-}

{-|
Module      : hnrmd
Description : hnrmd
Copyright   : (c) 2019, UChicago Argonne, LLC.
License     : BSD3
Maintainer  : fre@freux.fr
-}
module Main
  ( main
  )
where

import Nrm.Node.Hwloc (getHwlocData, selectCoreIDs, selectPUIDs)
import Protolude

main :: IO ()
main = do
  hwlocData <- getHwlocData
  print $ selectPUIDs hwlocData
  print $ selectCoreIDs hwlocData
