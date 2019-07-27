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
    getDefaultRaplDirs
  , defaultRaplDir
  , getRaplDirs
  , -- * Hwmon
    getDefaultHwmonDirs
  , defaultHwmonDir
  , getHwmonDirs
  )
where

import Nrm.Types.Topo
import Protolude
import System.Directory
import Text.RE.TDFA.Text

getDefaultRaplDirs :: IO [(PackageId, FilePath)]
getDefaultRaplDirs = getRaplDirs defaultRaplDir

defaultRaplDir :: FilePath
defaultRaplDir = "/sys/devices/virtual/powercap/intel-rapl"

-- | Lists available rapl directories.
getRaplDirs :: FilePath -> IO [(PackageId, FilePath)]
getRaplDirs = listDirFilter hasPackageIdNameFile

-- | Checks if the hwmon directory has "coretemp" in its name file.
hasPackageIdNameFile :: FilePath -> IO (Maybe (PackageId, FilePath))
hasPackageIdNameFile fp = do
  namecontent <- readFile (fp <> "/name")
  return $ (,fp <> "/name") <$> (matchedText (namecontent ?=~ rx) >>= idFromString . toS)
  where
    rx = [re|package-([0-9]+)(/\S+)?|]

-- | Lists hwmon directories at the default location.
getDefaultHwmonDirs :: IO [FilePath]
getDefaultHwmonDirs = getHwmonDirs defaultHwmonDir

-- | The default hwmon directory location
defaultHwmonDir :: FilePath
defaultHwmonDir = "/sys/class/hwmon/"

-- | Lists available hwmon directories.
getHwmonDirs :: FilePath -> IO [FilePath]
getHwmonDirs = listDirFilter hasCoretempInNameFile

-- | Checks if the hwmon directory has "coretemp" in its name file.
hasCoretempInNameFile :: FilePath -> IO (Maybe FilePath)
hasCoretempInNameFile fp =
  readFile (fp <> "/name") >>= \case
    "coretemp" -> return $ Just fp
    _ -> return Nothing

listDirFilter :: (FilePath -> IO (Maybe a)) -> FilePath -> IO [a]
listDirFilter condition basedir = listDirectory basedir >>= fmap catMaybes . mapM condition
