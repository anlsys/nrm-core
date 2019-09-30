{-|
Module      : NRM.Types.Topology.Package
Copyright   : (c) UChicago Argonne, 2019
License     : BSD3
Maintainer  : fre@freux.fr
-}
module NRM.Types.Topology.Package
  ( Package (..)
  , Rapl (..)
  )
where

import qualified CPD.Core as CPD
import Control.Lens
import Data.Aeson
import Data.Coerce
import Data.Data
import Data.Generics.Product
import Data.Map as DM
import Data.MessagePack
import LensMap.Core
import NRM.Classes.Actuators as AC
import NRM.Classes.Sensors as SC
import NRM.Node.Sysfs
import NRM.Node.Sysfs.Internal
import NRM.Types.Actuator
import NRM.Types.LMap as LM
import NRM.Types.Sensor
import NRM.Types.Topology.PackageID
import NRM.Types.Units
import Protolude hiding (max)

-- | Record containing all information about a CPU Package.
data Rapl
  = Rapl
      { raplPath :: FilePath
      , max :: MaxEnergy
      , frequency :: Frequency
      , discreteChoices :: [Power]
      }
  deriving (Show, Generic, Data, MessagePack, ToJSON, FromJSON)

newtype Package = Package {rapl :: Maybe Rapl}
  deriving (Show, Generic, Data, MessagePack, ToJSON, FromJSON)

instance HasLensMap (PackageID, Package) ActuatorKey Actuator where

  lenses (packageID, package) =
    DM.fromList
      [ ( AC.RaplKey packageID
        , ScopedLens (_2 . field @"rapl" . lens getter setter)
        )
      | _ <- Protolude.toList (rapl package)
      ]
    where
      getter (Just (Rapl path (MaxEnergy _maxEnergy) _freq discreteChoices)) =
        Just $ Actuator
          { actions = discreteChoices <&> fromuW
          , go = setRAPLPowercap path . RAPLCommand . uW
          }
      getter Nothing = Nothing
      setter (Just rapl) (Just (Actuator actions go)) =
        Just $ rapl & field @"discreteChoices" .~ fmap uW actions
      setter rapl Nothing = Nothing

instance HasLensMap (PackageID, Package) PassiveSensorKey PassiveSensor where

  lenses (packageID, package) =
    DM.fromList
      [ ( SC.RaplKey packageID
        , ScopedLens (_2 . field @"rapl" . lens getter setter)
        )
      | _ <- Protolude.toList (rapl package)
      ]
    where
      getter (Just (Rapl path (MaxEnergy maxEnergy) freq _discreteChoices)) =
        Just $ PassiveSensor
          { passiveTags = [Tag "power", Tag "RAPL"]
          , passiveSource = Source textID
          , passiveRange = (0, fromuJ maxEnergy)
          , frequency = freq
          , perform = measureRAPLDir path <&> fmap (fromuJ . energy)
          }
        where
          textID = show packageID
      getter Nothing = Nothing
      setter (Just rapl) (Just passiveSensor) =
        Just $ rapl & field @"max" .~ MaxEnergy (uJ (snd $ passiveRange passiveSensor))
      setter rapl Nothing = Nothing
