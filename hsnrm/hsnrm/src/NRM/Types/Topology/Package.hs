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
import Data.Maybe (fromJust)
import Data.MessagePack
import LMap.Map as LM
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
      Nothing -> LM.empty
      Just _ ->
        LM.singleton
          (A.RaplKey packageID)
          ( ScopedLens
              ( _2 . #rapl
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
      setter :: Rapl -> Actuator -> Rapl
      setter rapl (Actuator actions referenceAction _go) =
        rapl &~ do
          #discreteChoices .= fmap watts actions
          #defaultPower .= watts referenceAction

instance HasLensMap (PackageID, Package) S.PassiveSensorKey S.PassiveSensor where
  lenses (packageID, package) =
    rapl package & \case
      Nothing -> LM.empty
      Just _ ->
        LM.singleton
          (S.RaplKey packageID)
          ( ScopedLens
              ( _2 . #rapl
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
      setter :: Rapl -> S.PassiveSensor -> Rapl
      setter rapl passiveSensor =
        rapl &~ do
          #max .= passiveSensor ^. S._meta . #range . to (watts . sup)
          #history .= passiveSensor ^. S._meta . #lastReferenceMeasurements
          #lastRead .= (fmap joules <$> passiveSensor ^. S._meta . #last)
