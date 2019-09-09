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
  )
where

import Data.Aeson
import Data.Data
import Data.JSON.Schema
import Data.MessagePack
import Dhall
import NRM.Orphans.Dhall ()
import Protolude hiding ((%))

newtype Operations = Operations {fromOps :: Int}
  deriving (Eq, Ord, Show, Generic, Data, MessagePack, Interpret, Inject)
  deriving (JSONSchema, ToJSON, FromJSON) via Int

newtype Progress = Progress Int
  deriving (Show, Generic, Data, MessagePack)
  deriving (JSONSchema, ToJSON, FromJSON) via Int

newtype Frequency = Frequency {fromHz :: Double}
  deriving (Eq, Show, Generic, Data, Inject, Interpret, MessagePack)
  deriving (JSONSchema, ToJSON, FromJSON) via Double

newtype Power = Power {fromuW :: Double}
  deriving (Eq, Show, Generic, Data, Inject, Interpret, MessagePack)
  deriving (JSONSchema, ToJSON, FromJSON) via Double

newtype Time = Time {fromuS :: Double}
  deriving (Eq, Show, Generic, Data, Inject, Interpret, MessagePack)
  deriving (JSONSchema, ToJSON, FromJSON) via Double

newtype Energy = Energy {fromuJ :: Double}
  deriving (Eq, Show, Generic, Data, Inject, Interpret, MessagePack)
  deriving (JSONSchema, ToJSON, FromJSON) via Double

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
