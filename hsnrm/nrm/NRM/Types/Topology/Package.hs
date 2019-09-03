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
import CPD.Core
import NRM.Types.Sensor
import NRM.Types.Topology.PackageID
import NeatInterpolation
import Protolude hiding (max)

-- | Record containing all information about a CPU Package.
data RaplSensor
  = RaplSensor
      { id :: SensorID
      , raplPath :: FilePath
      , max :: MaxEnergy
      , frequency :: Double
      }
  deriving (Show, Generic, MessagePack, ToJSON, FromJSON)

instance CPDLSensor RaplSensor PackageID where

  toSensor packageID (RaplSensor id _path (MaxEnergy _energy) _freq) =
    ( id
    , Sensor
      { tags = [Tag "power", Tag "RAPL"]
      , source = Source $ textID
      , meta = Metadata (Interval 0 100) (FixedFrequency 2)
      , desc = Just $
        [text| "
          Intel RAPL sensor for package ID $textID .
          Values are given in uJ.
        |]
      }
    )
    where
      textID = show packageID


data Package
  = Package
      { raplSensor :: Maybe RaplSensor
      }
  deriving (Show, Generic, MessagePack, ToJSON, FromJSON)

instance HasSensors Package PackageID where

  toSensors packageID Package {..} = DM.fromList (toList $ toSensor packageID <$> raplSensor)
