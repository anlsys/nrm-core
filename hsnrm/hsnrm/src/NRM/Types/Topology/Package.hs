{-# OPTIONS_GHC -fno-warn-orphans #-}

-- |
-- Module      : NRM.Types.Topology.Package
-- Copyright   : (c) UChicago Argonne, 2019
-- License     : BSD3
-- Maintainer  : fre@freux.fr
module NRM.Types.Topology.Package
  ( Package (..),
    Rapl (..),
  )
where

import Control.Lens hiding ((...))
import Data.Aeson hiding ((.=))
import Data.Generics.Labels ()
import Data.Map as M
import Data.Maybe (fromJust)
import Data.MessagePack
import LensMap.Core
import NRM.Node.Sysfs
import NRM.Node.Sysfs.Internal
import NRM.Types.Actuator as A
import qualified NRM.Types.Configuration as Cfg
import NRM.Types.MemBuffer
import NRM.Types.Sensor as S
import NRM.Types.Topology.PackageID
import NRM.Types.Units
import Numeric.Interval
import Protolude hiding (max, to)

-- | Record containing all information about a CPU Package.
data Rapl
  = Rapl
      { raplCfg :: RAPLConfig,
        max :: Power,
        maxEnergyCounterValue :: Energy,
        frequency :: Frequency,
        discreteChoices :: [Power],
        defaultPower :: Power,
        lastRead :: Maybe (Time, Energy),
        history :: MemBuffer
      }
  deriving (Show, Generic, MessagePack, ToJSON, FromJSON)

newtype Package = Package {rapl :: Maybe Rapl}
  deriving (Show, Generic, MessagePack, ToJSON, FromJSON)

unsafeJAA :: p -> a -> Maybe a
unsafeJAA _ = Just

instance HasLensMap (PackageID, Package) ActuatorKey Actuator where
  lenses (packageID, package) =
    rapl package & \case
      Nothing -> M.empty
      Just _ ->
        M.singleton
          (A.RaplKey packageID)
          ( ScopedLens
              ( _2 . #rapl
                  . lens fromJust unsafeJAA
                  . lens getter setter
              )
          )
    where
      getter (NRM.Types.Topology.Package.Rapl raplCfg _maxPower _maxCounter _freq discreteChoices defaultPower _last _history) =
        Actuator
          { actions = discreteChoices <&> fromWatts,
            referenceAction = fromWatts defaultPower,
            go = setRAPLPowercapAllWindows raplCfg . watts
          }
      setter :: Rapl -> Actuator -> Rapl
      setter rapl (Actuator actions referenceAction _go) =
        rapl &~ do
          #discreteChoices .= fmap watts actions
          #defaultPower .= watts referenceAction

instance HasLensMap (PackageID, Package) S.PassiveSensorKey S.PassiveSensor where
  lenses (packageID, package) =
    rapl package & \case
      Nothing -> M.empty
      Just _ ->
        M.singleton
          (S.RaplKey packageID)
          ( ScopedLens
              ( _2 . #rapl
                  . lens fromJust unsafeJAA
                  . lens getter setter
              )
          )
    where
      getter (NRM.Types.Topology.Package.Rapl cfg maxPower maxCounter freq _discreteChoices _defaultPower lastRead history) =
        S.PassiveSensor
          { passiveMeta = S.SensorMeta
              { tags = [Cfg.Minimize, Cfg.TagPower, Cfg.Rapl],
                range = 0 ... fromWatts maxPower,
                S.lastReferenceMeasurements = history,
                last = lastRead <&> fmap fromJoules,
                cumulative = Cfg.CumulativeWithCapacity (fromJoules maxCounter)
              },
            frequency = freq,
            perform = measureRAPLDir (configPath cfg) <&> fmap (fromJoules . energy)
          }
      setter :: Rapl -> S.PassiveSensor -> Rapl
      setter rapl passiveSensor =
        rapl &~ do
          #max .= passiveSensor ^. S._meta . #range . to (watts . sup)
          #history .= passiveSensor ^. S._meta . #lastReferenceMeasurements
          #lastRead .= (fmap joules <$> passiveSensor ^. S._meta . #last)

deriving instance MessagePack RAPLConfig

deriving instance ToJSON RAPLConfig

deriving instance FromJSON RAPLConfig

deriving instance MessagePack RAPLConstraint

deriving instance ToJSON RAPLConstraint

deriving instance FromJSON RAPLConstraint

deriving instance MessagePack MaxPower

deriving instance ToJSON MaxPower

deriving instance FromJSON MaxPower
