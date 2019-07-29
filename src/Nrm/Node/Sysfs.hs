{-# LANGUAGE DuplicateRecordFields #-}

{-|
Module      : Nrm.Node.Sysfs
Description : Sysfs tree queries
Copyright   : (c) 2019, UChicago Argonne, LLC.
License     : BSD3
Maintainer  : fre@freux.fr
-}
module Nrm.Node.Sysfs
  ( -- * RAPL
    getDefaultRAPLDirs
  , readRAPLConfigurations
  , measureRAPLDirs
  , setRAPLPowercaps
  , -- * Hwmon
    getDefaultHwmonDirs
  )
where

import Nrm.Node.Internal.Sysfs
import Protolude

-- | Retreives RAPL directories at the default location.
getDefaultRAPLDirs :: IO RAPLDirs
getDefaultRAPLDirs = getRAPLDirs defaultRAPLDir

-- | Reads RAPL configurations.
readRAPLConfigurations :: RAPLDirs -> IO [RAPLConfig]
readRAPLConfigurations rapldirpaths = catMaybes <$> for rapldirpaths (readRAPLConfiguration . path)

-- | Performs RAPL measurements.
measureRAPLDirs :: RAPLDirs -> IO [RAPLMeasurement]
measureRAPLDirs rapldirpaths = catMaybes <$> for rapldirpaths (measureRAPLDir . path)

-- | Setting powercap values.
setRAPLPowercaps :: RAPLDirs -> RAPLCommands -> IO ()
setRAPLPowercaps rds = mapM_ (applyRAPLPcap rds)

-- | Retreives Hwmon directories at the default location.
getDefaultHwmonDirs :: IO HwmonDirs
getDefaultHwmonDirs = getHwmonDirs defaultHwmonDir
