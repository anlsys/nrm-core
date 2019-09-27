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
import Data.MessagePack
import NRM.Classes.Actuators
import NRM.Classes.Sensors
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
      { id :: SensorID
      , raplPath :: FilePath
      , max :: MaxEnergy
      , frequency :: Frequency
      }
  deriving (Show, Generic, Data, MessagePack, ToJSON, FromJSON)

raplToSensor :: Show a => a -> Rapl -> (SensorID, PassiveSensor)
raplToSensor packageID (Rapl id path (MaxEnergy maxEnergy) freq) =
  ( id
  , PassiveSensor
    { passiveTags = [Tag "power", Tag "RAPL"]
    , passiveSource = Source textID
    , passiveRange = (0, fromuJ maxEnergy)
    , frequency = freq
    , perform = measureRAPLDir path <&> fmap (fromuJ . energy)
    }
  )
  where
    textID = show packageID

raplToActuator :: a -> Rapl -> (CPD.ActuatorID, Actuator)
raplToActuator _packageID (Rapl id path (MaxEnergy _maxEnergy) _freq) =
  ( coerce id
  , Actuator
    { actions = [200, 220]
    , go = setRAPLPowercap path . RAPLCommand . uW
    }
  )

newtype Package = Package {rapl :: Maybe Rapl}
  deriving (Show, Generic, Data, MessagePack, ToJSON, FromJSON)

instance Actuators (PackageID, Package) where

  actuators (packageID, Package {..}) =
    LM.fromList
      ( Protolude.toList $
        raplToActuator packageID <$>
        rapl
      )

instance Sensors (PackageID, Package) where

  activeSensors _ = LM.empty

  passiveSensors (packageID, Package {..}) =
    LM.fromList
      ( Protolude.toList $
        raplToSensor packageID <$>
        rapl
      )

instance AdjustSensors (PackageID, Package) where

  adjust sensorID (CPD.Interval _ b) =
    _2 . field @"rapl" %~
      fmap
        ( \rapl@Rapl {..} ->
          if id == sensorID
          then rapl & field @"max" .~ MaxEnergy (uJ b)
          else rapl
        )
