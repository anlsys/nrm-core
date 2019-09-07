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
import Data.Generics.Product
import Data.Map as DM
import Lens.Micro
import qualified NRM.Types.Sensor as S
import NRM.Types.State
import Protolude

-- | List sensors
listNRMSensors :: NRMState -> Map SensorID Sensor
listNRMSensors s =
  DM.fromList $
    uncurry S.toCPDPackedSensor <$>
    DM.toList (S.listSensors () s)

-- | List sensors
listPackageSensors :: NRMState -> Map SensorID S.PackedSensor
listPackageSensors s =
  mconcat $ uncurry S.listSensors <$>
    DM.toList (packages s)

listDownstreamCmdSensors :: NRMState -> Map SensorID S.PackedSensor
listDownstreamCmdSensors s =
  mconcat $ uncurry S.listSensors <$>
    (DM.toList (cmdIDMap s) <&> \(cmdID, (cmd, _SliceID, _Slice)) -> (cmdID, cmd))

instance S.HasSensors NRMState () where

  listSensors _ s = listPackageSensors s <> listDownstreamCmdSensors s

  adjustRange sensorID range s =
    s &
      field @"packages" %~
      DM.map (S.adjustRange sensorID range) &
      field @"slices" %~
      fmap (field @"cmds" %~ DM.map (S.adjustRange sensorID range))
