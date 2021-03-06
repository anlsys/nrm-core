-- |
-- Module      : NRM.Node.Sysfs
-- Description : Sysfs tree queries
-- Copyright   : (c) 2019, UChicago Argonne, LLC.
-- License     : BSD3
-- Maintainer  : swann@anl.gov
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

import Data.Map as M
import NRM.Node.Sysfs.Internal
import NRM.Types.Units
import Protolude

-- | Retreives package RAPL directories at the default location.
getDefaultRAPLDirs :: FilePath -> IO (Maybe RAPLDirs)
getDefaultRAPLDirs = getRAPLDirs

-- | Performs RAPL measurements.
measureRAPLDirs :: RAPLDirs -> IO [RAPLMeasurement]
measureRAPLDirs (RAPLDirs rapldirpaths) = catMaybes <$> for (M.elems rapldirpaths) (measureRAPLDir . path)

-- | Setting powercap values.
setRAPLPowercapAllWindows :: RAPLConfig -> Power -> IO ()
setRAPLPowercapAllWindows = setRAPLPowercapByWindow [ShortTerm, LongTerm]

setRAPLPowercapByWindow :: Set Window -> RAPLConfig -> Power -> IO ()
setRAPLPowercapByWindow ws cfg p = applyRAPLPcap cfg (RAPLCommand {powercap = p, windows = ws})

-- | Retreives coretemp Hwmon directories at the default location.
getDefaultHwmonDirs :: FilePath -> IO HwmonDirs
getDefaultHwmonDirs = getHwmonDirs
