{-# OPTIONS_GHC -fno-warn-orphans #-}
{-|
Module      : NRM.Sensors
Copyright   : (c) 2019, UChicago Argonne, LLC.
License     : BSD3
Maintainer  : fre@freux.fr
-}
module NRM.Sensors
  ( listNRMSensors
  )
where

import CPD.Core
import Control.Lens
import Data.Generics.Product
import Data.Map as DM
import NRM.Classes.Sensors
import NRM.Types.State
import Protolude

listNRMSensors :: NRMState -> Map SensorID Sensor
listNRMSensors s =
  DM.fromList $
    uncurry toCPDPackedSensor <$>
    DM.toList (listSensors () s)

listPackageSensors :: NRMState -> Map SensorID PackedSensor
listPackageSensors s =
  mconcat $ uncurry listSensors <$>
    DM.toList (packages s)

listDownstreamCmdSensors :: NRMState -> Map SensorID PackedSensor
listDownstreamCmdSensors s =
  mconcat $ uncurry listSensors <$>
    (DM.toList (cmdIDMap s) <&> \(cmdID, (cmd, _SliceID, _Slice)) -> (cmdID, cmd))

instance HasSensors NRMState () where

  listSensors _ s = listPackageSensors s <> listDownstreamCmdSensors s

  adjustRange sensorID range s =
    s &
      field @"packages" %~
      DM.map (adjustRange sensorID range) &
      field @"slices" %~
      fmap (field @"cmds" %~ DM.map (adjustRange sensorID range))
