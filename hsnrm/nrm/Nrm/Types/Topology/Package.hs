{-|
Module      : Nrm.Types.Topology.Package
Copyright   : (c) UChicago Argonne, 2019
License     : BSD3
Maintainer  : fre@freux.fr
-}
module Nrm.Types.Topology.Package
  ( Package (..)
  , RaplSensor (..)
  )
where

import Data.Aeson
import Data.MessagePack
import Nrm.Node.Sysfs.Internal
import Protolude

-- | Record containing all information about a CPU Package.
data RaplSensor
  = RaplSensor
      { raplPath :: FilePath
      , max :: MaxEnergy
      }
  deriving (Show, Generic, MessagePack, ToJSON, FromJSON)

data Package
  = Package
      { raplSensor :: Maybe RaplSensor
      }
  deriving (Show, Generic, MessagePack, ToJSON, FromJSON)
