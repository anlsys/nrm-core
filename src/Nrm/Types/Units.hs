{-|
Module      : Nrm.Types.Units
Description : Units
Copyright   : (c) UChicago Argonne, 2019
License     : BSD3
Maintainer  : fre@freux.fr
-}
module Nrm.Types.Units
  ( Watt (..)
  , Power
  , uW
  )
where

import Data.Metrology as DM ((%))
import Data.Metrology.SI (Power)
import Data.Units.SI (Watt (..))
import Data.Units.SI.Prefixes (micro)
import Protolude

uW :: Double -> Power
uW = (DM.% micro Watt)
