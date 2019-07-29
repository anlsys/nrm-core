{-|
Module      : Nrm.Types.Units
Description : Units
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
  , uJ
  , uW
  , uS
  )
where

import Data.Metrology ((%))
import Data.Metrology.SI (Energy, Power, Time)
import Data.Units.SI (Joule (..), Second (..), Watt (..))
import Data.Units.SI.Prefixes (micro)
import Protolude hiding ((%))

-- | microjoule value constructor
uJ :: Double -> Energy
uJ = (% micro Joule)

-- | microwatt value constructor
uW :: Double -> Power
uW = (% micro Watt)

-- | microsecond value constructor
uS :: Double -> Time
uS = (% micro Second)