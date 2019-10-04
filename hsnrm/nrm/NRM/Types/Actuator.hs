{-# LANGUAGE DerivingVia #-}

{-|
Module      : NRM.Types.Actuator
Copyright   : (c) UChicago Argonne, 2019
License     : BSD3
Maintainer  : fre@freux.fr
-}
module NRM.Types.Actuator
  ( Actuator (..)
  , ActuatorKey (..)
  )
where

import Protolude
import NRM.Types.Topology.PackageID

data Actuator
  = Actuator
      { actions :: [Double]
      , go :: Double -> IO ()
      }

data ActuatorKey = RaplKey PackageID | A
  deriving (Show, Eq, Ord)
