{-|
Module      : NRM.Node.Sysfs
Description : Sysfs tree queries
Copyright   : (c) 2019, UChicago Argonne, LLC.
License     : BSD3
Maintainer  : fre@freux.fr
-}
module NRM.Node.Sysfs
  ( -- * RAPL
    RAPLDirs
  , getDefaultRAPLDirs
  , measureRAPLDirs
  , setRAPLPowercap
  , -- * Hwmon
    getDefaultHwmonDirs
  )
where

import NRM.Node.Sysfs.Internal
import NRM.Types.LMap as LM
import Protolude

-- | Retreives package RAPL directories at the default location.
getDefaultRAPLDirs :: FilePath -> IO (Maybe RAPLDirs)
getDefaultRAPLDirs = getRAPLDirs

-- | Performs RAPL measurements.
measureRAPLDirs :: RAPLDirs -> IO [RAPLMeasurement]
measureRAPLDirs (RAPLDirs rapldirpaths) = catMaybes <$> for (LM.elems rapldirpaths) (measureRAPLDir . path)

-- | Setting powercap values.
setRAPLPowercap :: FilePath -> RAPLCommand -> IO ()
setRAPLPowercap = applyRAPLPcap

-- | Retreives coretemp Hwmon directories at the default location.
getDefaultHwmonDirs :: FilePath -> IO HwmonDirs
getDefaultHwmonDirs = getHwmonDirs
