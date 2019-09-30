{-|
Module      : NRM.Classes.Actuators
Copyright   : (c) UChicago Argonne, 2019
License     : BSD3
Maintainer  : fre@freux.fr
-}
module NRM.Classes.Actuators
  ( toCPDActuator
  , ActuatorKey (..)
  )
where

import qualified CPD.Core as CPD
import NRM.Types.Actuator
import NRM.Types.LMap as LM
import NRM.Types.Topology.PackageID
import Protolude

toCPDActuator :: (ActuatorKey, Actuator) -> (CPD.ActuatorID, CPD.Actuator)
toCPDActuator (key, Actuator {..}) =
  ( CPD.ActuatorID $ show key
  , CPD.Actuator
    { CPD.actuatorRange = [CPD.Discrete (show x) | x <- actions]
    }
  )

data ActuatorKey = RaplKey PackageID | A
  deriving (Show, Eq, Ord)
