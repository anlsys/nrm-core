{-# LANGUAGE DerivingVia #-}
{-|
Module      : NRM.Types.Topology.PU
Copyright   : (c) UChicago Argonne, 2019
License     : BSD3
Maintainer  : fre@freux.fr
-}
module NRM.Types.Topology.PU
  ( PU (..)
  )
where

import Data.Aeson
import Data.MessagePack
import Data.Data
import NRM.Classes.Sensors
import NRM.Types.Topology.PUID
import Protolude

-- | Record containing all information about a processing unit.
data PU = PU
  deriving (Show, Generic,Data, MessagePack, ToJSON, FromJSON)

deriving via (NoSensors (PUID, PU)) instance Sensors (PUID, PU)

deriving via (NoSensors (PUID, PU)) instance AdjustSensors (PUID, PU)
