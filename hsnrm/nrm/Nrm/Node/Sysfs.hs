{-|
Module      : Nrm.Node.Sysfs
Description : Sysfs tree queries
Copyright   : (c) 2019, UChicago Argonne, LLC.
License     : BSD3
Maintainer  : fre@freux.fr
-}
module Nrm.Node.Sysfs
  ( -- * RAPL
    RAPLDirs
  , getDefaultRAPLDirs
  , readRAPLConfigurations
  , measureRAPLDirs
  {-, setRAPLPowercaps-}
  , -- * Hwmon
    getDefaultHwmonDirs
  )
where

import Nrm.Node.Sysfs.Internal
import Protolude

-- | Retreives package RAPL directories at the default location.
getDefaultRAPLDirs :: FilePath -> IO (Maybe RAPLDirs)
getDefaultRAPLDirs = getRAPLDirs

-- | Reads RAPL configurations.
readRAPLConfigurations :: RAPLDirs -> IO [RAPLConfig]
readRAPLConfigurations (RAPLDirs rapldirpaths) = catMaybes <$> for rapldirpaths (readRAPLConfiguration . path)

-- | Performs RAPL measurements.
measureRAPLDirs :: RAPLDirs -> IO [RAPLMeasurement]
measureRAPLDirs (RAPLDirs rapldirpaths) = catMaybes <$> for rapldirpaths (measureRAPLDir . path)

-- | Setting powercap values.
{-setRAPLPowercaps :: RAPLDirs -> RAPLCommands -> IO ()-}
{-setRAPLPowercaps rds = mapM_ (applyRAPLPcap rds)-}

-- | Retreives coretemp Hwmon directories at the default location.
getDefaultHwmonDirs :: FilePath -> IO HwmonDirs
getDefaultHwmonDirs = getHwmonDirs
