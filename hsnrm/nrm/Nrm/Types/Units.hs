{-#language FlexibleContexts#-}
{-#language UndecidableInstances#-}
{-|
Module      : Nrm.Types.Units
Copyright   : (c) UChicago Argonne, 2019
License     : BSD3
Maintainer  : fre@freux.fr
-}
module Nrm.Types.Units
  ( Joule
  , Watt
  , Second
  , Time
  , Energy
  , Power
  , Operations
  , Progress
  , uJ
  , uW
  , uS
  )
where

import Data.MessagePack
import Data.Metrology ((#), (%))
import qualified Data.Metrology.SI as DSI (Energy, Power, Time)
import Data.Units.SI (Joule (..), Second (..), Watt (..))
import Data.Units.SI.Prefixes (micro)
import Protolude hiding ((%))
import Prelude (Show)

newtype Operations = Operations Int

newtype Progress = Progress Int

newtype Energy = Energy DSI.Energy

newtype Power = Power DSI.Power

newtype Time = Time DSI.Time

deriving instance (Show DSI.Time) => Show Time

deriving instance (Show DSI.Power) => Show Power

deriving instance (Show DSI.Energy) => Show Energy

instance MessagePack Energy where

  toObject (Energy x) = toObject (x # micro Joule)

  fromObject x = fromObject x <&> uJ

-- | Microjoule value constructor.
uJ :: Double -> Energy
uJ x = Energy $ x % micro Joule

-- | Microwatt value constructor.
uW :: Double -> Power
uW x = Power $ x % micro Watt

-- | Microsecond value constructor.
uS :: Double -> Time
uS x = Time $ x % micro Second
