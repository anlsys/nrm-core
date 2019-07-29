{-# LANGUAGE LambdaCase #-}
{-# LANGUAGE QuasiQuotes #-}

{-|
Module      : Nrm.Node.Sysfs
Description : Sysfs tree queries
Copyright   : (c) 2019, UChicago Argonne, LLC.
License     : BSD3
Maintainer  : fre@freux.fr
-}
module Nrm.Node.Internal.Sysfs
  ( -- * RAPL
    RAPLDirs
  , RAPLDir (..)
  , RAPLConfig (..)
  , RAPLMeasurement (..)
  , RAPLConstraint (..)
  , MaxPower (..)
  , MaxEnergy (..)
  , defaultRAPLDir
  , getRAPLDirs
  , measureRAPLDir
  , readRAPLConfiguration
  , -- * Hwmon
    HwmonDirs
  , HwmonDir (..)
  , defaultHwmonDir
  , getHwmonDirs
  , hasCoretempInNameFile
  , -- * Utilities
    listDirFilter
  )
where

import Data.Metrology.Show ()
import Nrm.Types.Topo
import Nrm.Types.Units
import Protolude
import System.Directory
import Text.RE.TDFA.Text

-- | RAPL directory locations
type RAPLDirs = [RAPLDir]

-- | Hwmon directory locations
type HwmonDirs = [HwmonDir]

-- | Maximum RAPL power constraint.
newtype MaxPower = MaxPower Power
  deriving (Show)

-- | Maximum RAPL energy measurement.
newtype MaxEnergy = MaxEnergy Energy
  deriving (Show)

-- | RAPL energy measurement
newtype MeasuredEnergy = MeasuredEnergy Energy
  deriving (Show)

-- | Hwmon directory
newtype HwmonDir = HwmonDir FilePath
  deriving (Show)

-- | RAPL directory
data RAPLDir
  = RAPLDir
      { path :: FilePath
      , pkgid :: PackageId
      , maxEnergy :: MaxEnergy
      }
  deriving (Show)

-- | RAPL Configuration
data RAPLConfig
  = RAPLConfig
      { configPath :: FilePath
      , enabled :: Bool
      , constraintShortTerm :: RAPLConstraint
      , constraintLongTerm :: RAPLConstraint
      }
  deriving (Show)

-- | RAPL power constraint
data RAPLConstraint
  = RAPLConstraint
      { timeWindow :: Time
      , maxPower :: MaxPower
      }
  deriving (Show)

-- | RAPL power measurement
data RAPLMeasurement
  = RAPLMeasurement
      { measurementPath :: FilePath
      , energy :: Energy
      }

-- | Read configuration from a RAPL directory.
readRAPLConfiguration :: FilePath -> IO (Maybe RAPLConfig)
readRAPLConfiguration fp = do
  enabled :: Maybe Bool <- readMaybe . toS <$> readFile (fp <> "/enabled")
  name0 <- readFile (fp <> "/constraint_0_name")
  name1 <- readFile (fp <> "/constraint_1_name")
  constraint0 <- parseConstraint 0
  constraint1 <- parseConstraint 1
  return $ case (name0, name1) of
    ("short_term", "long_term") -> RAPLConfig fp <$> enabled <*> constraint1 <*> constraint0
    ("long_term", "short_term") -> RAPLConfig fp <$> enabled <*> constraint0 <*> constraint1
    _ -> Nothing
  where
    parseConstraint :: Int -> IO (Maybe RAPLConstraint)
    parseConstraint i = do
      maxpower <- readMaybe . toS <$> readFile (fp <> "/constraint_" <> show i <> "_max_power_uw")
      tw <- readMaybe . toS <$> readFile (fp <> "/constraint_" <> show i <> "_time_window")
      return (RAPLConstraint <$> (uS <$> tw) <*> (MaxPower . uW <$> maxpower))

-- | Measures power from a RAPL directory.
measureRAPLDir :: FilePath -> IO (Maybe RAPLMeasurement)
measureRAPLDir fp = do
  measured <- readMaybe . toS <$> readFile (fp <> "/energy_uj")
  return $ RAPLMeasurement fp . uJ <$> measured

-- | Checks if the hwmon directory has "coretemp" in its name file.
processRAPLFolder :: FilePath -> IO (Maybe RAPLDir)
processRAPLFolder fp = do
  namecontent <- readFile (fp <> "/name")
  maxRange <- readMaybe . toS <$> readFile (fp <> "/max_energy_range_uj")
  return $ RAPLDir (fp <> "/name") <$> (matchedText (namecontent ?=~ rx) >>= idFromString . toS) <*> (MaxEnergy . uJ <$> maxRange)
  where
    rx = [re|package-([0-9]+)(/\S+)?|]

-- | The default RAPL directory.
defaultRAPLDir :: FilePath
defaultRAPLDir = "/sys/devices/virtual/powercap/intel-rapl"

-- | Lists available rapl directories.
getRAPLDirs :: FilePath -> IO RAPLDirs
getRAPLDirs = listDirFilter processRAPLFolder

-- | "Utility": filter directories with monadic predicate.
listDirFilter :: (FilePath -> IO (Maybe a)) -> FilePath -> IO [a]
listDirFilter condition basedir = listDirectory basedir >>= fmap catMaybes . mapM condition

-- | Checks if the hwmon directory has "coretemp" in its name file.
hasCoretempInNameFile :: FilePath -> IO (Maybe HwmonDir)
hasCoretempInNameFile fp =
  readFile (fp <> "/name") >>= \case
    "coretemp" -> return $ Just (HwmonDir fp)
    _ -> return Nothing

-- | The default hwmon directory location
defaultHwmonDir :: FilePath
defaultHwmonDir = "/sys/class/hwmon/"

-- | Lists available hwmon directories.
getHwmonDirs :: FilePath -> IO HwmonDirs
getHwmonDirs = listDirFilter hasCoretempInNameFile
