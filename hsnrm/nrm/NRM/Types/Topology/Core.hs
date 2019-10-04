{-# LANGUAGE DerivingVia #-}

{-|
Module      : NRM.Types.Topology.Core
Copyright   : (c) UChicago Argonne, 2019
License     : BSD3
Maintainer  : fre@freux.fr
-}
module NRM.Types.Topology.Core
  ( Core (..)
  )
where

import Data.Aeson
import Data.Data
import Data.MessagePack
import NRM.Classes.Actuators
import NRM.Classes.Sensors
import NRM.Types.Topology.CoreID
import Protolude

-- | Record containing all information about a CPU Core.
data Core = Core
  deriving (Show, Generic, Data, MessagePack, ToJSON, FromJSON)
