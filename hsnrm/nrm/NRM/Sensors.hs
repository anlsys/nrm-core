{-# LANGUAGE DerivingVia #-}

{-# OPTIONS_GHC -fno-warn-orphans #-}
{-|
Module      : NRM.Sensors
Copyright   : (c) 2019, UChicago Argonne, LLC.
License     : BSD3
Maintainer  : fre@freux.fr
-}
module NRM.Sensors
  ( cpdSensors
  )
where

import CPD.Core as CPD
import Control.Lens
import Data.Generics.Product
import LMap.Map as LM
import NRM.Classes.Sensors
import NRM.Types.State
import Protolude

cpdSensors = undefined
