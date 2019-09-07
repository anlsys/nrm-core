{-# LANGUAGE DerivingVia #-}

{-# OPTIONS_GHC -fno-warn-orphans #-}

{-|
Module      : NRM.Types.Units
Copyright   : (c) UChicago Argonne, 2019
License     : BSD3
Maintainer  : fre@freux.fr
-}
module NRM.Types.Units
  ( -- * Operations
    Operations (..)
  , -- * Application Progress
    Progress (..)
  , -- * Frequency
    Frequency
  , hz
  , fromHz
  , -- * Time
    Time
  , uS
  , fromuS
  , -- * Energy
    Energy
  , uJ
  , fromuJ
  , -- * Power
    Power
  , uW
  , fromuW
  , watts
  , fromWatts
  )
where

import Data.Aeson
import Data.Data
import Data.JSON.Schema
import Data.MessagePack
import Dhall
import NRM.Classes.Messaging
import NRM.Orphans.Dhall ()
import Protolude hiding ((%))

-- | CPU operations.
newtype Operations = Operations {ops :: Int}
  deriving (Eq, Ord, Show, Generic, Data, MessagePack, Interpret, Inject)
  deriving (JSONSchema, ToJSON, FromJSON) via GenericJSON Operations

-- | Application progress.
newtype Progress = Progress Int
  deriving (Show, Generic, Data, MessagePack)
  deriving (JSONSchema, ToJSON, FromJSON) via GenericJSON Progress

-- | Frequency newtype for Data.UNITS.SI Frequency
newtype Frequency = Frequency Double
  deriving (Eq, Show, Generic, Data, Inject, Interpret, MessagePack)
  deriving (JSONSchema, ToJSON, FromJSON) via Double

-- | Frequency newtype for Data.UNITS.SI Frequency
newtype Power = Power Double
  deriving (Eq, Show, Generic, Data, Inject, Interpret, MessagePack)
  deriving (JSONSchema, ToJSON, FromJSON) via Double

-- | Frequency newtype for Data.UNITS.SI Frequency
newtype Time = Time Double
  deriving (Eq, Show, Generic, Data, Inject, Interpret, MessagePack)
  deriving (JSONSchema, ToJSON, FromJSON) via Double

newtype Energy = Energy Double
  deriving (Eq, Show, Generic, Data, Inject, Interpret, MessagePack)
  deriving (JSONSchema, ToJSON, FromJSON) via Double

fromuJ :: Energy -> Double
fromuJ (Energy x) = x

fromuS :: Time -> Double
fromuS (Time x) = x

fromuW :: Power -> Double
fromuW (Power x) = x

fromWatts :: Power -> Double
fromWatts (Power x) = x

fromHz :: Frequency -> Double
fromHz (Frequency x) = x

-- | Microjoule value constructor.
uJ :: Double -> Energy
uJ = Energy

-- | Microwatt value constructor.
uW :: Double -> Power
uW = Power

-- | Watt value constructor.
watts :: Double -> Power
watts = Power . (* 1000.0)

-- | Microsecond value constructor.
uS :: Double -> Time
uS = Time

-- | Hertz value constructor.
hz :: Double -> Frequency
hz = Frequency
