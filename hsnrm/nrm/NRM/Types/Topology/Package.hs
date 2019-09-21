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

import qualified CPD.Core as CPD
import Control.Lens
import Data.Aeson
import Data.Data
import Data.Generics.Product
import qualified Data.Map as DM
import Data.MessagePack
import NRM.Classes.Sensors
import NRM.Node.Sysfs.Internal
import NRM.Types.Sensor
import NRM.Types.Topology.PackageID
import NRM.Types.Units
import Protolude hiding (max)

-- | Record containing all information about a CPU Package.
data RaplSensor
  = RaplSensor
      { id :: SensorID
      , raplPath :: FilePath
      , max :: MaxEnergy
      , frequency :: Frequency
      }
  deriving (Show, Generic, Data, MessagePack, ToJSON, FromJSON)

raplToSensor :: Show a => a -> RaplSensor -> (SensorID, Sensor)
raplToSensor packageID (RaplSensor id path (MaxEnergy maxEnergy) freq) =
  ( id
  , Passive $ PassiveSensor
    { passiveTags = [Tag "power", Tag "RAPL"]
    , passiveSource = Source textID
    , passiveRange = (0, fromuJ maxEnergy)
    , frequency = freq
    , perform = measureRAPLDir path <&> fmap (fromuJ . energy)
    }
  )
  where
    textID = show packageID

newtype Package
  = Package
      { raplSensor :: Maybe RaplSensor
      }
  deriving (Show, Generic, Data, MessagePack, ToJSON, FromJSON)

instance HasSensors Package PackageID where

  listSensors packageID Package {..} =
    DM.fromList
      ( toList $ (\(x, y) -> (x, packSensor y)) .
        raplToSensor packageID <$>
        raplSensor
      )

  adjustRange sensorID (CPD.Interval _ b) p =
    p & field @"raplSensor" %~
      fmap
        ( \rapl@RaplSensor {..} ->
          if id == sensorID
          then rapl & field @"max" .~ MaxEnergy (uJ b)
          else rapl
        )
