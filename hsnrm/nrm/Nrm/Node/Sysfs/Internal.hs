{-# LANGUAGE LambdaCase #-}
{-# LANGUAGE QuasiQuotes #-}

{-|
Module      : Nrm.Node.Sysfs
Copyright   : (c) 2019, UChicago Argonne, LLC.
License     : BSD3
Maintainer  : fre@freux.fr
-}
module Nrm.Node.Sysfs.Internal
  ( -- * RAPL
    RAPLDir (..)
  , RAPLDirs (..)
  , RAPLConfig (..)
  , RAPLMeasurement (..)
  , RAPLConstraint (..)
  , RAPLCommand (..)
  , RAPLCommands (..)
  , MaxPower (..)
  , MaxEnergy (..)
  , defaultRAPLDir
  , getRAPLDirs
  , measureRAPLDir
  , readRAPLConfiguration
  , {-, applyRAPLPcap-}
    -- * Hwmon
    HwmonDirs
  , HwmonDir (..)
  , defaultHwmonDir
  , getHwmonDirs
  , hasCoretempInNameFile
  , -- * Utilities
    listDirFilter
  )
where

import Control.Monad.Trans.Maybe
import Data.MessagePack
import Data.Metrology.Show ()
import Data.Text as T (length, lines)
import Nrm.Types.Topology
import Nrm.Types.Units
import Protolude
import System.Directory
import Text.RE.TDFA.Text

-- | RAPL directory locations
newtype RAPLDirs = RAPLDirs [RAPLDir]
  deriving (Show, Generic, MessagePack)

-- | Hwmon directory locations
newtype HwmonDirs = HwmonDirs [HwmonDir]
  deriving (Show, Generic)

-- | RAPL Powercap command
newtype RAPLCommands = RAPLCommands [RAPLCommand]

-- | Maximum RAPL power constraint.
newtype MaxPower = MaxPower Power
  deriving (Show)

-- | Maximum RAPL energy measurement.
newtype MaxEnergy = MaxEnergy Energy
  deriving (Show, Generic, MessagePack)

{-deriving instance MessagePack MaxEnergy via Int-}

{-instance MessagePack MaxEnergy where-}

{-toObject (PackageID x) = toObject (unrefine x)-}

{-fromObject x =-}
{-(fromObject x <&> refine) >>= \case-}
{-Right r -> return $ PackageID r-}
{-Left _ -> fail "Couldn't refine PackageID during MsgPack conversion"-}

-- | RAPL energy measurement
newtype MeasuredEnergy = MeasuredEnergy Energy
  deriving (Show)

-- | Hwmon directory
newtype HwmonDir = HwmonDir FilePath
  deriving (Show)

data RAPLCommand
  = RAPLCommand
      { commandPkgid :: PackageID
      , powercap :: Power
      }
  deriving (Show)

-- | RAPL directory
data RAPLDir
  = RAPLDir
      { path :: FilePath
      , pkgid :: PackageID
      , maxEnergy :: MaxEnergy
      }
  deriving (Show, Generic, MessagePack)

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
  deriving (Show)

-- | The default RAPL directory.
defaultRAPLDir :: FilePath
defaultRAPLDir = "/sys/devices/virtual/powercap/intel-rapl"

-- | The default hwmon directory location
defaultHwmonDir :: FilePath
defaultHwmonDir = "/sys/class/hwmon"

-- | Read configuration from a RAPL directory.
readRAPLConfiguration :: FilePath -> IO (Maybe RAPLConfig)
readRAPLConfiguration fp =
  runMaybeT $ do
    enabled <- (== ("1" :: Text)) <$> maybeTReadLine (fp <> "/enabled")
    name0 <- maybeTReadLine (fp <> "/constraint_0_name")
    name1 <- maybeTReadLine (fp <> "/constraint_1_name")
    constraint0 <- parseConstraint 0
    constraint1 <- parseConstraint 1
    case (name0, name1) of
      ("short_term", "long_term") -> return $ RAPLConfig fp enabled constraint0 constraint1
      ("long_term", "short_term") -> return $ RAPLConfig fp enabled constraint1 constraint0
      _ -> mzero
  where
    parseConstraint :: Int -> MaybeT IO RAPLConstraint
    parseConstraint i = do
      maxpower <- maybeTReadLine (fp <> "/constraint_" <> show i <> "_max_power_uw") >>= (MaybeT . pure . readMaybe . toS)
      tw <- maybeTReadLine (fp <> "/constraint_" <> show i <> "_time_window_us") >>= (MaybeT . pure . readMaybe . toS)
      return $ RAPLConstraint (uS tw) (MaxPower . uW $ maxpower)

-- | Measures power from a RAPL directory.
measureRAPLDir :: (MonadIO m) => FilePath -> m (Maybe RAPLMeasurement)
measureRAPLDir fp =
  runMaybeT $ do
    content <- maybeTReadLine $ fp <> "/energy_uj"
    measured <- MaybeT . pure . readMaybe $ toS content
    return $ RAPLMeasurement fp (uJ measured)

-- | Checks if the hwmon directory has "coretemp" in its name file.
processRAPLFolder :: (MonadIO m) => FilePath -> m (Maybe RAPLDir)
processRAPLFolder fp =
  runMaybeT $ do
    namecontent <- maybeTReadLine $ fp <> "/name"
    maxRange <- maybeTReadLine (fp <> "/max_energy_range_uj") >>= (MaybeT . pure . readMaybe . toS)
    match <- MaybeT $ pure (matchedText (namecontent ?=~ rx) >>= idFromString . drop (T.length "package") . toS)
    return $ RAPLDir fp match (MaxEnergy . uJ $ maxRange)
  where
    rx = [re|package-([0-9]+)(/\S+)?|]

-- | Applies powercap commands.
{-applyRAPLPcap :: RAPLDirs -> RAPLCommand -> IO ()-}
{-applyRAPLPcap _ RAPLCommand {..} = return ()-}

-- | Lists available rapl directories.
getRAPLDirs :: FilePath -> IO RAPLDirs
getRAPLDirs d = RAPLDirs <$> listDirFilter processRAPLFolder d

-- | "Utility": filter directories with monadic predicate.
listDirFilter :: (FilePath -> IO (Maybe a)) -> FilePath -> IO [a]
listDirFilter condition basedir = ((((basedir <> "/") <>) <$>) <$> listDirectory basedir) >>= fmap catMaybes . mapM condition

-- | Checks if the hwmon directory has "coretemp" in its name file.
hasCoretempInNameFile :: FilePath -> IO (Maybe HwmonDir)
hasCoretempInNameFile fp =
  runMaybeT $
    maybeTReadLine (fp <> "/name") >>= \case
    "coretemp" -> MaybeT $ pure $ Just $ HwmonDir fp
    _ -> mzero

{-where-}
{-f :: Either SomeException Text -> IO (Maybe HwmonDir)-}
{-f (Right "coretemp") = return . Just $ HwmonDir fp-}
{-f _ = return Nothing-}

-- | Lists available hwmon directories.
getHwmonDirs :: FilePath -> IO HwmonDirs
getHwmonDirs fp = HwmonDirs <$> listDirFilter hasCoretempInNameFile fp

maybeTReadLine :: (MonadIO m) => FilePath -> MaybeT m Text
maybeTReadLine fp = maybeTReadFile fp >>= MaybeT . pure . head . lines

maybeTReadFile :: (MonadIO m) => FilePath -> MaybeT m Text
maybeTReadFile fp = MaybeT . liftIO $ maybeReadFile fp

maybeReadFile :: FilePath -> IO (Maybe Text)
maybeReadFile fpath = (hush :: Either SomeException Text -> Maybe Text) <$> try (readFile fpath)
