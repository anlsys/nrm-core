{-# LANGUAGE DerivingVia #-}
{-# OPTIONS_GHC -fno-warn-orphans #-}

-- |
-- Module      : NRM.Types.Units
-- Copyright   : (c) UChicago Argonne, 2019
-- License     : BSD3
-- Maintainer  : fre@freux.fr
module NRM.Types.Units
  ( -- * Operations
    Operations (..),

    -- * Application Progress
    Progress,
    fromProgress,
    progress,

    -- * Frequency
    Frequency,
    hz,
    fromHz,

    -- * Time
    Time,
    uS,
    seconds,
    fromuS,
    fromSeconds,

    -- * Energy
    Energy,
    uJ,
    fromuJ,

    -- * Power
    Power,
    uW,
    fromuW,
    watts,
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
  deriving (Eq, Ord, Generic, Data, MessagePack, Interpret, Inject)
  deriving (Show, JSONSchema, ToJSON, FromJSON) via Int

newtype Progress = Progress {fromProgress :: Int}
  deriving (Generic, Data, MessagePack)
  deriving (Show, JSONSchema, ToJSON, FromJSON) via Int

newtype Frequency = Frequency {fromHz :: Double}
  deriving (Eq,Ord, Generic, Data, Inject, Interpret, MessagePack)
  deriving (Show, JSONSchema, ToJSON, FromJSON) via Double

newtype Power = Power {fromuW :: Double}
  deriving (Eq, Generic, Data, Inject, Interpret, MessagePack)
  deriving (Show, JSONSchema, ToJSON, FromJSON) via Double

newtype Time = Time {fromuS :: Double}
  deriving (Eq, Generic, Data, Inject, Interpret, MessagePack)
  deriving (Show, JSONSchema, ToJSON, FromJSON) via Double

newtype Energy = Energy {fromuJ :: Double}
  deriving (Eq, Generic, Data, Inject, Interpret, MessagePack)
  deriving (Show, JSONSchema, ToJSON, FromJSON) via Double

-- | Microjoule value constructor.
progress :: Int -> Progress
progress = Progress

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

-- | Second value constructor.
seconds :: Double -> Time
seconds = Time . (* 1000000)

fromSeconds :: Time -> Double
fromSeconds (Time t) = t / 1000000

-- | Hertz value constructor.
hz :: Double -> Frequency
hz = Frequency
