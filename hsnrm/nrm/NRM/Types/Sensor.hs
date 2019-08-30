{-# LANGUAGE DerivingVia #-}

{-|

Module      : NRM.Types.Sensor
Copyright   : (c) UChicago Argonne, 2019
License     : BSD3
Maintainer  : fre@freux.fr
-}
module NRM.Types.Sensor
  (
  PassiveSensor(..)
  ,ActiveSensor(..)
  )
where

import Data.Aeson
import Data.MessagePack
import NRM.Types.Topology.Package
import Protolude

-- Internal view
newtype PackageSensor = TagRaplSensor RaplSensor
  deriving (Show, Generic, MessagePack, FromJSON, ToJSON)

class PassiveSensor a where

  maxFrequency :: a -> Double

  validate :: a -> Value

class ActiveSensor a where

  frequency :: a -> Double

  perform :: a -> Value
