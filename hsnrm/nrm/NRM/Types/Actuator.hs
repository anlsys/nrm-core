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

import qualified CPD.Core as CPD
import NRM.Classes.Actuators
import NRM.Types.Topology.PackageID
import Protolude

data Actuator
  = Actuator
      { actions :: [Double]
      , go :: Double -> IO ()
      }

data ActuatorKey = RaplKey PackageID | A
  deriving (Show, Eq, Ord)

instance StringConv ActuatorKey CPD.ActuatorID where

  strConv _ = CPD.ActuatorID . show

instance ToCPDActuator ActuatorKey Actuator where

  toCPDActuator (id, Actuator {..}) =
    ( toS id
    , CPD.Actuator
      { actions = CPD.DiscreteDouble <$> actions
      }
    )
