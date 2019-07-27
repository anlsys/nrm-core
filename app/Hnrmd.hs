{-# LANGUAGE NoImplicitPrelude #-}

{-|
Module      : hnrmd
Description : hnrmd
Copyright   : (c) 2019, UChicago Argonne, LLC.
License     : BSD3
Maintainer  : fre@freux.fr
-}
module Hnrmd
  ( main
  )
where

import Nrm.Node.Hwloc
import Nrm.Node.Sysfs
import Protolude

main :: IO ()
main = do
  hwlocData <- getHwlocData
  putText "Internal representation check..."
  putText "PUs:"
  print $ selectPUIDs hwlocData
  putText "Cores:"
  print $ selectCoreIDs hwlocData
  putText "Packages:"
  print $ selectPackageIDs hwlocData
  putText "Locating relevant directories."
  raplDir <- getDefaultRaplDirs
  hwmonDir <- getDefaultHwmonDirs
  print raplDir
  print hwmonDir
