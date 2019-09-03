{-# LANGUAGE DerivingVia #-}

{-|
Module      : CPD.Text
Copyright   : (c) UChicago Argonne, 2019
License     : BSD3
Maintainer  : fre@freux.fr
-}
module CPD.Text
  ( showSensor
  , showSensors
  , showActuator
  , showActuators
  )
where

import CPD.Core
import qualified Data.Map as DM
import Protolude

showSensors :: DM.Map SensorID Sensor -> Text
showSensors sl = mconcat $ (\(id, x) -> showSensor id x <> "\n ") <$> DM.toList sl

showSensor :: SensorID -> Sensor -> Text
showSensor id Sensor {..} =
  "ID  " <> show id <> "Source  " <> show source <>
    " tags:" <>
    (mconcat . intersperse " " $ show <$> sensorTags) <>
    " " <>
    show sensorMeta <>
    " " <>
    show sensorDesc <>
    " \n"

showActuators :: Map ActuatorID Actuator -> Text
showActuators sl = mconcat $ (\(id, x) -> showActuator id x <> "\n ") <$> DM.toList sl

showActuator :: ActuatorID -> Actuator -> Text
showActuator _ _ = "  "
