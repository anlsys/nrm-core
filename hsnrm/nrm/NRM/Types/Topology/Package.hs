{-# OPTIONS_GHC -fno-warn-incomplete-uni-patterns #-}
{-# OPTIONS_GHC -fno-warn-missing-local-signatures #-}

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
import Data.Generics.Product
import Data.Maybe (fromJust)
import Data.MessagePack
import LMap.Map as DM
import LensMap.Core
import NRM.Node.Sysfs
import NRM.Node.Sysfs.Internal
import NRM.Types.Actuator as A
import NRM.Types.MemBuffer
import NRM.Types.Sensor as S
import NRM.Types.Topology.PackageID
import NRM.Types.Units
import Numeric.Interval
import Protolude hiding (max, to)

-- | Record containing all information about a CPU Package.
data Rapl
  = Rapl
      { raplPath :: FilePath,
        max :: Power,
        maxEnergyCounterValue :: Energy,
        frequency :: Frequency,
        discreteChoices :: [Power],
        defaultPower :: Power,
        lastRead :: Maybe (Time, Energy),
        history :: MemBuffer Double
      }
  deriving (Show, Generic, MessagePack, ToJSON, FromJSON)

newtype Package = Package {rapl :: Maybe Rapl}
  deriving (Show, Generic, MessagePack, ToJSON, FromJSON)

unsafeJAA _ = Just

instance HasLensMap (PackageID, Package) ActuatorKey Actuator where
  lenses (packageID, package) =
    rapl package & \case
      Nothing -> DM.empty
      Just _ ->
        DM.singleton
          (A.RaplKey packageID)
          ( ScopedLens
              ( _2 . field @"rapl"
                  . lens fromJust unsafeJAA
                  . lens getter setter
              )
          )
    where
      getter (NRM.Types.Topology.Package.Rapl path _maxPower _maxCounter _freq discreteChoices defaultPower _last _history) =
        Actuator
          { actions = discreteChoices <&> fromWatts,
            referenceAction = fromWatts defaultPower,
            go = setRAPLPowercap path . RAPLCommand . watts
          }
      setter rapl (Actuator actions referenceAction _go) =
        rapl &~ do
          field @"discreteChoices" .= fmap watts actions
          field @"defaultPower" .= watts referenceAction

instance HasLensMap (PackageID, Package) S.PassiveSensorKey S.PassiveSensor where
  lenses (packageID, package) =
    rapl package & \case
      Nothing -> DM.empty
      Just _ ->
        DM.singleton
          (S.RaplKey packageID)
          ( ScopedLens
              ( _2 . field @"rapl"
                  . lens fromJust unsafeJAA
                  . lens getter setter
              )
          )
    where
      getter (NRM.Types.Topology.Package.Rapl path maxPower maxCounter freq _discreteChoices _defaultPower lastRead history) =
        S.PassiveSensor
          { passiveMeta = S.SensorMeta
              { tags = [S.Minimize, S.Power, S.Rapl],
                range = 0 ... fromWatts maxPower,
                S.lastReferenceMeasurements = history,
                last = lastRead <&> fmap fromJoules,
                cumulative = S.CumulativeWithCapacity (fromJoules maxCounter)
              },
            frequency = freq,
            perform = measureRAPLDir path <&> fmap (fromJoules . energy)
          }
      setter rapl passiveSensor =
        rapl &~ do
          field @"max" .= passiveSensor ^. S._meta . field @"range" . to (watts . sup)
          field @"history" .= passiveSensor ^. S._meta . field @"lastReferenceMeasurements"
          field @"lastRead" .= (fmap joules <$> passiveSensor ^. S._meta . field @"last")
