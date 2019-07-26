{-# LANGUAGE QuasiQuotes #-}

{-|
Module      : Nrm.Node.Sysfs
Description : Sysfs tree queries
Copyright   : (c) 2019, UChicago Argonne, LLC.
License     : BSD3
Maintainer  : fre@freux.fr
-}
module Nrm.Node.Sysfs
  ( -- * Hwmon
    defaultGetHwmonDirs
  , defaultHwmonDir
  , getHwmonDirs
  )
where

import Nrm.Types.Topo (PackageId)
import Protolude
import System.Directory
import Text.RE.TDFA.Text

defaultRaplDir :: FilePath
defaultRaplDir = "/sys/devices/virtual/powercap/intel-rapl"

getRaplDirs :: FilePath -> IO [FilePath]
getRaplDirs basedir = listDirectory basedir >>= fmap catMaybes . mapM hasCoretempInNameFile

-- | Checks if the hwmon directory has "coretemp" in its name file.
hasPackageIdNameFile :: FilePath -> IO (Maybe PackageId)
hasPackageIdNameFile fp =
  readFile (fp <> "/name") >>= \case
    "coretemp" -> return $ Just fp
    _ -> return Nothing

-- | Lists hwmon directories at the default location.
defaultGetHwmonDirs :: IO [FilePath]
defaultGetHwmonDirs = getHwmonDirs defaultHwmonDir

-- | The default hwmon directory location
defaultHwmonDir :: FilePath
defaultHwmonDir = "/sys/class/hwmon/"

-- | Lists available hwmon directories.
getHwmonDirs :: FilePath -> IO [FilePath]
getHwmonDirs basedir = listDirectory basedir >>= fmap catMaybes . mapM hasCoretempInNameFile

-- | Checks if the hwmon directory has "coretemp" in its name file.
hasCoretempInNameFile :: FilePath -> IO (Maybe FilePath)
hasCoretempInNameFile fp =
  readFile (fp <> "/name") >>= \case
    "coretemp" -> return $ Just fp
    _ -> return Nothing
