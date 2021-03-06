{-# LANGUAGE DerivingVia #-}

-- |
-- Module      : NRM.Types.Actuator
-- Copyright   : (c) UChicago Argonne, 2019
-- License     : BSD3
-- Maintainer  : swann@anl.gov
module NRM.Types.Actuator
  ( Actuator (..),
    ActuatorKey (..),
    fromCPDKey,
  )
where

import qualified CPD.Core as CPD
import NRM.Classes.Actuators
import NRM.Types.Topology.PackageID
import Protolude

data Actuator
  = Actuator
      { actions :: [Double],
        referenceAction :: Double,
        go :: Double -> IO ()
      }
  deriving (Generic)

data ActuatorKey = RaplKey PackageID | ExtraActuatorKey Text | CmdActuatorKey Text
  deriving (Show, Read, Eq, Ord)

instance StringConv ActuatorKey CPD.ActuatorID where
  strConv _ = CPD.ActuatorID . show

fromCPDKey :: CPD.ActuatorID -> Maybe ActuatorKey
fromCPDKey (CPD.ActuatorID id) = readMaybe (toS id)

instance ToCPDActuator ActuatorKey Actuator where
  toCPDActuator (id, Actuator {..}) =
    ( toS id,
      CPD.Actuator
        { actions = CPD.DiscreteDouble <$> actions
        }
    )
