{-|
Module      : NRM.Classes.Actuators
Copyright   : (c) UChicago Argonne, 2019
License     : BSD3
Maintainer  : fre@freux.fr
-}
module NRM.Classes.Actuators
  ( toCPDActuator
  , Actuators (..)
  , NoActuators (..)
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

-- Structural
class Actuators a where

  actuators :: a -> LMap ActuatorKey Actuator

newtype NoActuators (a :: Type) = NoActuators {unNoActuators :: a}

instance Actuators (NoActuators a) where

  actuators = const LM.empty

-- Actuator maps
instance (Actuators (k, v)) => Actuators (LMap k v) where

  actuators (LM.toList -> m) = mconcat (m <&> actuators)

instance (Actuators a) => Actuators (Maybe a) where

  actuators (Just x) = actuators x
  actuators Nothing = LM.empty

data ActuatorKey = RaplKey PackageID | A deriving (Show)
