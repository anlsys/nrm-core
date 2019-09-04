{-|
Module      : NRM.Types.Topology.Package
Copyright   : (c) UChicago Argonne, 2019
License     : BSD3
Maintainer  : fre@freux.fr
-}
module NRM.Types.Topology.Package
  ( Package (..)
  , RaplSensor (..)
  )
where

import Data.Aeson
import qualified Data.Map as DM
import Data.MessagePack
import NRM.Node.Sysfs.Internal
import NRM.Types.Sensor
import NRM.Types.Topology.PackageID
import NRM.Types.Units
import NeatInterpolation
import Protolude hiding (max)

-- | Record containing all information about a CPU Package.
data RaplSensor
  = RaplSensor
      { id :: SensorID
      , raplPath :: FilePath
      , max :: MaxEnergy
      , frequency :: Frequency
      }
  deriving (Show, Generic, MessagePack, ToJSON, FromJSON)

raplToSensor :: forall k a1 (a2 :: k). Show a1 => a1 -> RaplSensor -> (SensorID, Sensor a2)
raplToSensor packageID (RaplSensor id path (MaxEnergy maxEnergy) freq) =
  ( id
  , PassiveSensor
    { sensorTags = [Tag "power", Tag "RAPL"]
    , source = Source textID
    , range = (0, fromuJ maxEnergy)
    , frequency = freq
    , sensorDesc = Just
      [text| "
          Intel RAPL sensor for package ID $textID .
          Values are given in uJ.
        |]
    , perform = measureRAPLDir path <&> fmap (fromuJ . energy)
    }
  )
  where
    textID = show packageID

newtype Package
  = Package
      { raplSensor :: Maybe RaplSensor
      }
  deriving (Show, Generic, MessagePack, ToJSON, FromJSON)

instance HasSensors Package PackageID where

  listSensors packageID Package {..} =
    DM.fromList
      (toList $ (\(x, y) -> (x, packSensor y)) . raplToSensor packageID <$> raplSensor)