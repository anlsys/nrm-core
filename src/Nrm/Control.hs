{-|
Module      : Nrm.Control
Description : Control
Copyright   : (c) UChicago Argonne, 2019
License     : BSD3
Maintainer  : fre@freux.fr
-}
module Nrm.Control
  ( Sensor(..)
  , Actuator(..)
  )
where

import Protolude

class Sensor config measurement where
  doSensor :: config -> IO measurement

class Actuator config action where
  doAction :: config -> action -> IO ()
