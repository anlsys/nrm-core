-- |
-- Module      : NRM.Node.Sysfs
-- Description : Sysfs tree queries
-- Copyright   : (c) 2019, UChicago Argonne, LLC.
-- License     : BSD3
-- Maintainer  : fre@freux.fr
module NRM.Node.Sysfs
  ( -- * RAPL
    RAPLDirs,
    getDefaultRAPLDirs,
    measureRAPLDirs,
    setRAPLPowercapAllWindows,
    setRAPLPowercapByWindow,

    -- * Hwmon
    getDefaultHwmonDirs,
  )
where

import LMap.Map as LM
import NRM.Node.Sysfs.Internal
import NRM.Types.Units
import Protolude

-- | Retreives package RAPL directories at the default location.
getDefaultRAPLDirs :: FilePath -> IO (Maybe RAPLDirs)
getDefaultRAPLDirs = getRAPLDirs

-- | Performs RAPL measurements.
measureRAPLDirs :: RAPLDirs -> IO [RAPLMeasurement]
measureRAPLDirs (RAPLDirs rapldirpaths) = catMaybes <$> for (LM.elems rapldirpaths) (measureRAPLDir . path)

-- | Setting powercap values.
setRAPLPowercapAllWindows :: RAPLConfig -> Power -> IO ()
setRAPLPowercapAllWindows = setRAPLPowercapByWindow [ShortTerm, LongTerm]

setRAPLPowercapByWindow :: Set Window -> RAPLConfig -> Power -> IO ()
setRAPLPowercapByWindow ws cfg p = applyRAPLPcap cfg (RAPLCommand {powercap = p, windows = ws})

-- | Retreives coretemp Hwmon directories at the default location.
getDefaultHwmonDirs :: FilePath -> IO HwmonDirs
getDefaultHwmonDirs = getHwmonDirs
