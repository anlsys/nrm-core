-- |
-- Module      : NRM.Actuators
-- Copyright   : (c) 2019, UChicago Argonne, LLC.
-- License     : BSD3
-- Maintainer  : swann@anl.gov
module NRM.Actuators
  ( cpdActuators,
  )
where

import qualified CPD.Core as CPD
import Control.Lens
import Data.Map as M
import LensMap.Core
import NRM.Classes.Actuators
import NRM.Types.Actuator
import NRM.Types.State
import Protolude hiding (Map)

cpdActuators :: NRMState -> Map CPD.ActuatorID CPD.Actuator
cpdActuators st =
  M.fromList $
    M.toList
      (lenses st :: LensMap NRMState ActuatorKey Actuator)
      <&> \(k, ScopedLens sl) -> toCPDActuator (k, view sl st)
