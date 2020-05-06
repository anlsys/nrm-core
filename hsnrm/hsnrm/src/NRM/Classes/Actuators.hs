-- |
-- Module      : NRM.Classes.Actuators
-- Copyright   : (c) UChicago Argonne, 2019
-- License     : BSD3
-- Maintainer  : fre@freux.fr
module NRM.Classes.Actuators
  ( ToCPDActuator (..),
  )
where

import qualified CPD.Core as CPD

class ToCPDActuator k a where
  toCPDActuator :: (k, a) -> (CPD.ActuatorID, CPD.Actuator)
