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

-- | The main daemon process
main :: IO ()
main = do
  hwlocData <- getHwlocData
  putText "Internal representation check..."
  putText "\nPUs:"
  print $ selectPUIDs hwlocData
  putText "\nCores:"
  print $ selectCoreIDs hwlocData
  putText "\nPackages:"
  print $ selectPackageIDs hwlocData
  putText "\nLocating relevant (package related) RAPL directories."
  raplDirs <- getDefaultRAPLDirs
  print raplDirs
  putText "\nReading RAPL configurations."
  raplConfig <- readRAPLConfigurations raplDirs
  print raplConfig
  putText "\nPerforming RAPL measurement."
  raplMeasurement <- measureRAPLDirs raplDirs
  print raplMeasurement
  putText "\nLocating relevant (coretemp related) Hwmon directories."
  hwmonDir <- getDefaultHwmonDirs
  print hwmonDir
