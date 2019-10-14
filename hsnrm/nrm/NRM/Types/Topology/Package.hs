{-# OPTIONS_GHC -fno-warn-missing-local-signatures #-}
{-# OPTIONS_GHC -fno-warn-incomplete-uni-patterns #-}
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

import Control.Lens
import Data.Aeson
import Data.Data
import Data.Generics.Product
import Data.Map as DM
import Data.Maybe (fromJust)
import Data.MessagePack
import LensMap.Core
import NRM.Node.Sysfs
import NRM.Node.Sysfs.Internal
import NRM.Types.Actuator as A
import NRM.Types.Sensor as S
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
      , lastTime :: Maybe (Time, Energy)
      }
  deriving (Show, Generic, Data, MessagePack, ToJSON, FromJSON)

newtype Package = Package {rapl :: Maybe Rapl}
  deriving (Show, Generic, Data, MessagePack, ToJSON, FromJSON)

instance HasLensMap (PackageID, Package) ActuatorKey Actuator where

  lenses (packageID, package) =
    rapl package & \case
      Nothing -> DM.empty
      Just _ ->
        DM.singleton
          (A.RaplKey packageID)
          ( ScopedLens
            ( _2 . field @"rapl" .
              (lens fromJust \(Just _) a -> Just a) . --TODO refactor for -fwarn-uni-patterns
              lens getter setter
            )
          )
    where
      getter (Rapl path (MaxEnergy _maxEnergy) _freq discreteChoices _last) =
        Actuator
          { actions = discreteChoices <&> fromuW
          , go = setRAPLPowercap path . RAPLCommand . uW
          }
      setter rapl (Actuator actions _go) =
        rapl & field @"discreteChoices" .~ fmap uW actions

instance HasLensMap (PackageID, Package) PassiveSensorKey PassiveSensor where

  lenses (packageID, package) =
    rapl package & \case
      Nothing -> DM.empty
      Just _ ->
        DM.singleton
          (S.RaplKey packageID)
          ( ScopedLens
            ( _2 . field @"rapl" .
              (lens fromJust \(Just _) a -> Just a) .
              lens getter setter
            )
          )
    where
      getter (Rapl path (MaxEnergy maxEnergy) freq _discreteChoices last) =
        PassiveSensor
          { passiveTags = [Tag "power", Tag "RAPL"]
          , passiveSource = Source textID
          , passiveRange = (0, fromuJ maxEnergy)
          , frequency = freq
          , perform = measureRAPLDir path <&> fmap (fromuJ . energy)
          , last = last <&> fmap fromuJ
          }
        where
          textID = show packageID
      setter rapl passiveSensor =
        rapl & field @"max" .~ MaxEnergy (uJ (snd $ passiveRange passiveSensor))
