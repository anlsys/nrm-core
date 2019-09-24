{-# LANGUAGE DerivingVia #-}

{-|
Module      : NRM.Types.Actuator
Copyright   : (c) UChicago Argonne, 2019
License     : BSD3
Maintainer  : fre@freux.fr
-}
module NRM.Types.Actuator
  ( Actuator (..)
  )
where

import Protolude

data Actuator
  = Actuator
      { actions :: [Double]
      , go :: Double -> IO ()
      }
