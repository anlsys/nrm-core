{-|
Module      : Nrm.Types.Units
Copyright   : (c) UChicago Argonne, 2019
License     : BSD3
Maintainer  : fre@freux.fr
-}
module Nrm.Types.Units
  ( -- * Operations
    Operations (..)
  , -- * Application Progress
    Progress (..)
  , -- * Time
    Time
  , Second
  , uS
  , -- * Energy
    Energy
  , Joule
  , uJ
  , -- * Power
    Power
  , Watt
  , uW
  , watts
  )
where

import Data.Aeson
import Data.JSON.Schema
import Data.MessagePack
import Data.Metrology ((#), (%))
import Data.Metrology.Poly (showIn)
import qualified Data.Metrology.SI as DSI (Energy, Power, Time)
import Data.Units.SI (Joule (..), Second (..), Watt (..))
import Data.Units.SI.Prefixes (micro)
import Generics.Generic.Aeson
import Protolude hiding ((%))
import qualified Prelude as PBase

-- | CPU operations.
newtype Operations = Operations Int
  deriving (Show, Generic, MessagePack)

instance ToJSON Operations where

  toJSON = gtoJson

instance FromJSON Operations where

  parseJSON = gparseJson

instance JSONSchema Operations where

  schema = gSchema

-- | Application progress.
newtype Progress = Progress Int
  deriving (Show, Generic, MessagePack)

instance ToJSON Progress where

  toJSON = gtoJson

instance FromJSON Progress where

  parseJSON = gparseJson

instance JSONSchema Progress where

  schema = gSchema

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
