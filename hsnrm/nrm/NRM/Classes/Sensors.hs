{-# LANGUAGE RankNTypes #-}

-- |
-- Module      : NRM.Classes.Sensors
-- Copyright   : (c) UChicago Argonne, 2019
-- License     : BSD3
-- Maintainer  : fre@freux.fr
module NRM.Classes.Sensors
  ( ToCPDSensor (..),
  )
where

import qualified CPD.Core as CPD

class ToCPDSensor k a where
  toCPDSensor :: (k, a) -> (CPD.SensorID, CPD.Sensor)
