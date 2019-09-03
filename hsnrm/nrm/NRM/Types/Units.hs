{-# LANGUAGE DerivingVia #-}

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
  , Hertz
  , hz
  , fromHz
  , -- * Time
    Time
  , Second
  , uS
  , fromuS
  , -- * Energy
    Energy
  , Joule
  , uJ
  , fromuJ
  , -- * Power
    Power
  , Watt
  , uW
  , fromuW
  , watts
  , fromWatts
  )
where

import Data.Aeson
import Data.Functor.Contravariant (contramap)
import Data.JSON.Schema
import Data.MessagePack
import Data.Metrology ((#), (%))
import Data.Metrology.Poly (showIn)
import qualified Data.Metrology.SI as DSI (Energy, Frequency, Power, Time)
import Data.Units.SI (Hertz (..), Joule (..), Second (..), Watt (..))
import Data.Units.SI.Prefixes (micro)
import Dhall
import NRM.Classes.Messaging
import Protolude hiding ((%))
import qualified Prelude as PBase

-- | CPU operations.
newtype Operations = Operations {ops :: Int}
  deriving (Show, Generic, MessagePack)
  deriving (JSONSchema, ToJSON, FromJSON) via GenericJSON Operations

-- | Application progress.
newtype Progress = Progress Int
  deriving (Show, Generic, MessagePack)
  deriving (JSONSchema, ToJSON, FromJSON) via GenericJSON Progress

-- | Frequency newtype for Data.UNITS.SI Frequency
newtype Frequency = Frequency DSI.Frequency
  deriving (Eq, Generic)

instance Dhall.Inject Frequency where

  injectWith = fmap (contramap fromHz) Dhall.injectWith

instance Interpret Frequency where

  autoWith = fmap hz . autoWith

instance ToJSON Frequency where

  toJSON (Frequency x) = toJSON (x # Hertz)

instance FromJSON Frequency where

  parseJSON = fmap hz . parseJSON

instance JSONSchema Frequency where

  schema _ = schema (Proxy :: Proxy Double)

instance PBase.Show Frequency where

  show (Frequency x) = showIn x Hertz

instance MessagePack Frequency where

  toObject (Frequency x) = toObject (x # Hertz)

  fromObject x = fromObject x <&> hz

-- | Power newtype for Data.UNITS.SI Power
newtype Power = Power DSI.Power

instance ToJSON Power where

  toJSON (Power x) = toJSON (x # micro Watt)

instance FromJSON Power where

  parseJSON = fmap uW . parseJSON

instance JSONSchema Power where

  schema _ = schema (Proxy :: Proxy Double)

instance PBase.Show Power where

  show (Power x) = showIn x (micro Watt)

instance MessagePack Power where

  toObject (Power x) = toObject (x # micro Watt)

  fromObject x = fromObject x <&> uW

-- | Time newtype for Data.UNITS.SI Time
newtype Time = Time DSI.Time

instance ToJSON Time where

  toJSON (Time x) = toJSON (x # micro Second)

instance FromJSON Time where

  parseJSON = fmap uS . parseJSON

instance JSONSchema Time where

  schema _ = schema (Proxy :: Proxy Double)

instance PBase.Show Time where

  show (Time x) = showIn x (micro Second)

instance MessagePack Time where

  toObject (Time x) = toObject (x # micro Second)

  fromObject x = fromObject x <&> uS

-- | Unit newtype for Data.UNITS.SI Energy
newtype Energy = Energy DSI.Energy

instance ToJSON Energy where

  toJSON (Energy x) = toJSON (x # micro Joule)

instance FromJSON Energy where

  parseJSON = fmap uJ . parseJSON

instance JSONSchema Energy where

  schema _ = schema (Proxy :: Proxy Double)

instance PBase.Show Energy where

  show (Energy x) = showIn x (micro Joule)

instance MessagePack Energy where

  toObject (Energy x) = toObject (x # micro Joule)

  fromObject x = fromObject x <&> uJ

fromuJ :: Energy -> Double
fromuJ (Energy x) = x # micro Joule

fromuS :: Time -> Double
fromuS (Time x) = x # micro Second

fromuW :: Power -> Double
fromuW (Power x) = x # micro Watt

fromWatts :: Power -> Double
fromWatts (Power x) = x # Watt

fromHz :: Frequency -> Double
fromHz (Frequency x) = x # Hertz

-- | Microjoule value constructor.
uJ :: Double -> Energy
uJ x = Energy $ x % micro Joule

-- | Microwatt value constructor.
uW :: Double -> Power
uW x = Power $ x % micro Watt

-- | Watt value constructor.
watts :: Double -> Power
watts x = Power $ x % Watt

-- | Microsecond value constructor.
uS :: Double -> Time
uS x = Time $ x % micro Second

-- | Hertz value constructor.
hz :: Double -> Frequency
hz x = Frequency $ x % Hertz
