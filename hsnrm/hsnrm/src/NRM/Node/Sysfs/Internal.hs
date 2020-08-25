{-# LANGUAGE QuasiQuotes #-}

-- |
-- Module      : NRM.Node.Sysfs
-- Copyright   : (c) 2019, UChicago Argonne, LLC.
-- License     : BSD3
-- Maintainer  : fre@freux.fr
module NRM.Node.Sysfs.Internal
  ( -- * RAPL
    RAPLDir (..),
    RAPLDirs (..),
    RAPLConfig (..),
    RAPLMeasurement (..),
    RAPLConstraint (..),
    RAPLCommand (..),
    MaxPower (..),
    MaxEnergy (..),
    Window (..),
    getRAPLDirs,
    measureRAPLDir,
    readRAPLConfiguration,
    applyRAPLPcap,
    processRAPLFolder,

    -- * Hwmon
    HwmonDirs,
    HwmonDir (..),
    getHwmonDirs,
    hasCoretempInNameFile,

    -- * Utilities
    listDirFilter,
  )
where

import Control.Lens hiding (re)
import Control.Monad.Trans.Maybe
import Data.Aeson
import Data.Data
import Data.Generics.Labels ()
import Data.MessagePack
import Data.Metrology.Show ()
import Data.Text as T (length, lines)
import qualified Data.Map as M
import NRM.Types.Topology.PackageID
import NRM.Types.Units
import Protolude
import System.Directory
import Text.RE.TDFA.Text

-- | RAPL directory locations
newtype RAPLDirs = RAPLDirs (M.Map PackageID RAPLDir)
  deriving (Show, Generic, MessagePack)

-- | Hwmon directory locations
newtype HwmonDirs = HwmonDirs [HwmonDir]
  deriving (Show, Generic)

-- | Maximum RAPL power constraint.
newtype MaxPower = MaxPower Power
  deriving (Show, Generic)

-- | Maximum RAPL energy measurement.
newtype MaxEnergy = MaxEnergy Energy
  deriving (Show, Generic, Data, MessagePack, ToJSON, FromJSON)

-- | RAPL energy measurement
newtype MeasuredEnergy = MeasuredEnergy Energy
  deriving (Show, Generic)

-- | Hwmon directory
newtype HwmonDir = HwmonDir FilePath
  deriving (Show, Generic)

data RAPLCommand
  = RAPLCommand
      { powercap :: Power,
        windows :: Set Window
      }
  deriving (Show, Generic)

-- | RAPL directory
data RAPLDir
  = RAPLDir
      { path :: FilePath,
        maxEnergy :: Energy
      }
  deriving (Show, Generic, MessagePack)

-- | RAPL Configuration
data RAPLConfig
  = RAPLConfig
      { configPath :: FilePath,
        enabled :: Bool,
        constraintShortTermMaxPowerUw :: RAPLConstraint,
        constraintLongTermMaxPowerUw :: RAPLConstraint,
        shortTermID :: Int,
        longTermID :: Int
      }
  deriving (Show, Generic)

-- | RAPL power constraint
data RAPLConstraint
  = RAPLConstraint
      { timeWindow :: Time,
        maxPower :: MaxPower
      }
  deriving (Show, Generic)

-- | RAPL power measurement
data RAPLMeasurement
  = RAPLMeasurement
      { measurementPath :: FilePath,
        energy :: Energy
      }
  deriving (Show, Generic)

data Window = ShortTerm | LongTerm deriving (Show, Generic, Ord, Eq)

-- | Read configuration from a RAPL directory.
readRAPLConfiguration :: FilePath -> IO (Maybe RAPLConfig)
readRAPLConfiguration fp =
  runMaybeT $ do
    enabled <- (== ("1" :: Text)) <$> maybeTReadLine (fp <> "/enabled")
    names <- forOf both (0 :: Int, 1 :: Int) $ \i ->
      maybeTReadLine (fp <> "/constraint_" <> show i <> "_name")
    maxConstraints <- forOf both (0, 1) parseConstraint
    let flippedC = case names of
          ("short_term", "long_term") -> False
          ("long_term", "short_term") -> True
          _ -> panic "rapl constraint name files contain unexpected values"
        (shortTerm, longTerm) = (if flippedC then swap else identity) maxConstraints
    return RAPLConfig
      { configPath = fp,
        enabled = enabled,
        constraintShortTermMaxPowerUw = shortTerm,
        constraintLongTermMaxPowerUw = longTerm,
        shortTermID = fromEnum flippedC,
        longTermID = fromEnum (not flippedC)
      }
  where
    parseConstraint :: Int -> MaybeT IO RAPLConstraint
    parseConstraint i = do
      maxpower <-
        maybeTReadLine
          ( fp
              <> "/constraint_"
              <> show i
              <> "_max_power_uw"
          )
          >>= (MaybeT . pure . readMaybe . toS)
      tw <-
        maybeTReadLine
          ( fp <> "/constraint_"
              <> show i
              <> "_time_window_us"
          )
          >>= (MaybeT . pure . readMaybe . toS)
      return $ RAPLConstraint (uS tw) (MaxPower . uW $ maxpower)

-- | Measures power from a RAPL directory.
measureRAPLDir :: (MonadIO m) => FilePath -> m (Maybe RAPLMeasurement)
measureRAPLDir fp =
  runMaybeT $ do
    content <- maybeTReadLine $ fp <> "/energy_uj"
    measured <- MaybeT . pure . readMaybe $ toS content
    return $ RAPLMeasurement fp (uJ measured)

-- | Checks if the hwmon directory has "coretemp" in its name file.
processRAPLFolder :: (MonadIO m) => FilePath -> m (Maybe (PackageID, RAPLDir))
processRAPLFolder fp =
  runMaybeT $ do
    namecontent <- maybeTReadLine $ fp <> "/name"
    maxRange <- maybeTReadLine (fp <> "/max_energy_range_uj") >>= (MaybeT . pure . readMaybe . toS)
    match <- MaybeT $ pure (matchedText (namecontent ?=~ rx) >>= fmap PackageID . readMaybe . drop (T.length "package-") . toS)
    return (match, RAPLDir fp (uJ maxRange))
  where
    rx = [re|package-([0-9]+)(/\S+)?|]

-- | Applies powercap commands.
applyRAPLPcap :: RAPLConfig -> RAPLCommand -> IO ()
applyRAPLPcap raplCfg (RAPLCommand cap windows) = for_ windows $ \w ->
  writeFile
    (configPath raplCfg <> toS (windowToPath w))
    (show . (floor :: Double -> Int) $ fromuW cap)
  where
    windowToPath :: Window -> Text
    windowToPath w = "/constraint_" <> show (raplCfg ^. getID w) <> "_power_limit_uw"
    getID :: Window -> Getting Int RAPLConfig Int
    getID = \case
      ShortTerm -> #shortTermID
      LongTerm -> #longTermID

-- | Lists available rapl directories.
getRAPLDirs :: FilePath -> IO (Maybe RAPLDirs)
getRAPLDirs d =
  try (RAPLDirs . M.fromList <$> listDirFilter processRAPLFolder d) <&> \case
    Left (SomeException _) -> Nothing
    Right dirs -> Just dirs

-- | "Utility": filter directories with monadic predicate.
listDirFilter :: (FilePath -> IO (Maybe a)) -> FilePath -> IO [a]
listDirFilter condition basedir = ((((basedir <> "/") <>) <$>) <$> listDirectory basedir) >>= fmap catMaybes . mapM condition

-- | Checks if the hwmon directory has "coretemp" in its name file.
hasCoretempInNameFile :: FilePath -> IO (Maybe HwmonDir)
hasCoretempInNameFile fp =
  runMaybeT $
    maybeTReadLine (fp <> "/name") >>= \case
      "coretemp" -> MaybeT . pure . Just $ HwmonDir fp
      _ -> mzero

-- | Lists available hwmon directories.
getHwmonDirs :: FilePath -> IO HwmonDirs
getHwmonDirs fp = HwmonDirs <$> listDirFilter hasCoretempInNameFile fp

maybeTReadLine :: (MonadIO m) => FilePath -> MaybeT m Text
maybeTReadLine fp = maybeTReadFile fp >>= MaybeT . pure . head . lines

maybeTReadFile :: (MonadIO m) => FilePath -> MaybeT m Text
maybeTReadFile fp = MaybeT . liftIO $ maybeReadFile fp

maybeReadFile :: FilePath -> IO (Maybe Text)
maybeReadFile fpath = (hush :: Either SomeException Text -> Maybe Text) <$> try (readFile fpath)
