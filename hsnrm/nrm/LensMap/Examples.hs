{-# LANGUAGE RankNTypes #-}

{-|
Module      : LensMap.Examples
Copyright   : (c) UChicago Argonne, 2019
License     : BSD3
Maintainer  : fre@freux.fr
-}
module LensMap.Examples where

import Control.Lens
import Data.Generics.Product
import LensMap.Core
import Protolude

data SensorKey = SensorKey
  deriving (Ord, Eq)

data MySensor = MySensor

data A = A

data B = B

data C = C

newtype Machin = Machin {getMachin :: Map Int C}
  deriving (Generic)

data MyState
  = MyState
      { truc :: Map Int A
      , chose :: Map Int B
      , machin :: Map Int Machin
      }
  deriving (Generic)

instance HasLensMap (Int, A) SensorKey MySensor

instance HasLensMap (Int, B) SensorKey MySensor

instance HasLensMap MyState SensorKey MySensor where

  lenses st =
    mconcat
      [ addPath (field @"truc") <$> lenses (truc st)
      , addPath (field @"chose") <$> lenses (chose st)
      ]
