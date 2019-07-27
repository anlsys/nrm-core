{-# LANGUAGE LambdaCase #-}
{-# LANGUAGE QuasiQuotes #-}

{-|
Module      : Nrm.Node.Sysfs
Description : Sysfs tree queries
Copyright   : (c) 2019, UChicago Argonne, LLC.
License     : BSD3
Maintainer  : fre@freux.fr
-}
module Nrm.Node.Sysfs
  ( -- * RAPL
    RaplDirectories (..)
  , getDefaultRaplDirs
  , defaultRaplDir
  , getRaplDirs
  , -- * Hwmon
    HwmonDirectories (..)
  , getDefaultHwmonDirs
  , defaultHwmonDir
  , getHwmonDirs
  )
where

import Data.Metrology.Show ()
import Nrm.Types.Topo
import Nrm.Types.Units
import Protolude
import System.Directory
import Text.RE.TDFA.Text

newtype RaplDirectories = RaplDirectories [(PackageId, FilePath, MaxPower)]
  deriving (Show)

newtype HwmonDirectories = HwmonDirectories [FilePath]
  deriving (Show)

newtype MaxPower = MaxPower Power
  deriving (Show)

getDefaultRaplDirs :: IO RaplDirectories
getDefaultRaplDirs = getRaplDirs defaultRaplDir

defaultRaplDir :: FilePath
defaultRaplDir = "/sys/devices/virtual/powercap/intel-rapl"

-- | Lists available rapl directories.
getRaplDirs :: FilePath -> IO RaplDirectories
getRaplDirs basedir = RaplDirectories <$> listDirFilter hasPackageIdNameFile basedir

-- | Checks if the hwmon directory has "coretemp" in its name file.
hasPackageIdNameFile :: FilePath -> IO (Maybe (PackageId, FilePath, MaxPower))
hasPackageIdNameFile fp = do
  namecontent <- readFile (fp <> "/name")
  maxRange <- readMaybe . toS <$> readFile (fp <> "/max_energy_range_uj")
  return $ (,fp <> "/name",) <$> (matchedText (namecontent ?=~ rx) >>= idFromString . toS) <*> (MaxPower . uW <$> maxRange)
  where
    rx = [re|package-([0-9]+)(/\S+)?|]

-- | Lists hwmon directories at the default location.
getDefaultHwmonDirs :: IO HwmonDirectories
getDefaultHwmonDirs = getHwmonDirs defaultHwmonDir

-- | The default hwmon directory location
defaultHwmonDir :: FilePath
defaultHwmonDir = "/sys/class/hwmon/"

-- | Lists available hwmon directories.
getHwmonDirs :: FilePath -> IO HwmonDirectories
getHwmonDirs basepath = HwmonDirectories <$> listDirFilter hasCoretempInNameFile basepath

-- | Checks if the hwmon directory has "coretemp" in its name file.
hasCoretempInNameFile :: FilePath -> IO (Maybe FilePath)
hasCoretempInNameFile fp =
  readFile (fp <> "/name") >>= \case
    "coretemp" -> return $ Just fp
    _ -> return Nothing

listDirFilter :: (FilePath -> IO (Maybe a)) -> FilePath -> IO [a]
listDirFilter condition basedir = listDirectory basedir >>= fmap catMaybes . mapM condition
