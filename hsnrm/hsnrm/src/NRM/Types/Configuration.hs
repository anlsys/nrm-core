{-# LANGUAGE DerivingVia #-}
{-# OPTIONS_GHC -fno-warn-partial-fields #-}

-- |
-- Module      : NRM.Types.Configuration
-- Copyright   : (c) UChicago Argonne, 2019
-- License     : BSD3
-- Maintainer  : fre@freux.fr
module NRM.Types.Configuration
  ( Cfg (..),
    SliceRuntime (..),
    UpstreamCfg (..),
    DownstreamCfg (..),
    DaemonVerbosity (..),
    ControlCfg (..),
    RaplCfg (..),
    HwmonCfg (..),
    ExtraPassiveSensor (..),
    ExtraActuator (..),
    jsonOptions,
    examples,
  )
where

import CPD.Core
import Data.Aeson
import Data.Default
import Data.JSON.Schema
import Data.MessagePack
import Data.Yaml.Internal ()
import Dhall
import NRM.Classes.Messaging
import NRM.Orphans.Dhall ()
import qualified NRM.Types.Cmd as Cmd
import NRM.Types.Controller
import NRM.Types.Sensor
import NRM.Types.Units
import Numeric.Interval
import Protolude
import Refined
import Refined.Unsafe

data SliceRuntime = Singularity | Nodeos | Dummy
  deriving (Eq, Show, Generic, MessagePack, Interpret, Inject)
  deriving (JSONSchema, ToJSON, FromJSON) via GenericJSON SliceRuntime

data DaemonVerbosity = Error | Info | Debug
  deriving (Eq, Ord, Show, Generic, MessagePack, Interpret, Inject)
  deriving (JSONSchema, ToJSON, FromJSON) via GenericJSON DaemonVerbosity

data Cfg
  = Cfg
      { verbose :: DaemonVerbosity,
        logfile :: Text,
        hwloc :: Text,
        perf :: Text,
        argo_perf_wrapper :: Cmd.Command,
        argo_nodeos_config :: Cmd.Command,
        libnrmPath :: Maybe Text,
        pmpi_lib :: Text,
        singularity :: Bool,
        dummy :: Bool,
        nodeos :: Bool,
        slice_runtime :: SliceRuntime,
        downstreamCfg :: DownstreamCfg,
        upstreamCfg :: UpstreamCfg,
        raplCfg :: Maybe RaplCfg,
        hwmonCfg :: HwmonCfg,
        controlCfg :: ControlCfg,
        passiveSensorFrequency :: Frequency,
        extraStaticPassiveSensors :: Map Text ExtraPassiveSensor,
        extraStaticActuators :: Map Text ExtraActuator
      }
  deriving (Show, Generic, MessagePack, Interpret, Inject)
  deriving (JSONSchema, ToJSON, FromJSON) via GenericJSON Cfg

data ExtraActuator
  = ExtraActuator
      { actuatorBinary :: Text,
        actuatorArguments :: [Text],
        actions :: [Discrete],
        referenceAction :: Discrete
      }
  deriving (Eq, Show, Generic, MessagePack, Interpret, Inject)
  deriving (JSONSchema, ToJSON, FromJSON) via GenericJSON ExtraActuator

data ExtraPassiveSensor
  = ExtraPassiveSensor
      { sensorBinary :: Text,
        sensorArguments :: [Text],
        range :: Interval Double,
        tags :: [Tag],
        sensorBehavior :: Cumulative
      }
  deriving (Eq, Show, Generic, MessagePack, Interpret, Inject)
  deriving (JSONSchema, ToJSON, FromJSON) via GenericJSON ExtraPassiveSensor

data ControlCfg
  = ControlCfg
      { minimumControlInterval :: Time,
        minimumWaitInterval :: Time,
        staticPower :: Power,
        learnCfg :: LearnConfig,
        speedThreshold :: Double,
        referenceMeasurementRoundInterval :: Refined (GreaterThan 5) Int,
        hint :: Hint
      }
  | NoControl
  deriving (Eq, Show, Generic, MessagePack, Interpret, Inject)
  deriving (JSONSchema, ToJSON, FromJSON) via GenericJSON ControlCfg

data HwmonCfg
  = HwmonCfg
      { hwmonEnabled :: Bool,
        hwmonPath :: Text
      }
  deriving (Eq, Show, Generic, MessagePack, Interpret, Inject)
  deriving (JSONSchema, ToJSON, FromJSON) via GenericJSON HwmonCfg

data RaplCfg
  = RaplCfg
      { raplPath :: Text,
        raplActions :: [Power],
        referencePower :: Power
      }
  deriving (Eq, Show, Generic, MessagePack, Interpret, Inject)
  deriving (JSONSchema, ToJSON, FromJSON) via GenericJSON RaplCfg

newtype DownstreamCfg
  = DownstreamCfg
      { downstreamBindAddress :: Text
      }
  deriving (Eq, Show, Generic, MessagePack, Interpret, Inject)
  deriving (JSONSchema, ToJSON, FromJSON) via GenericJSON DownstreamCfg

data UpstreamCfg
  = UpstreamCfg
      { upstreamBindAddress :: Text,
        pubPort :: Int,
        rpcPort :: Int
      }
  deriving (Eq, Show, Generic, MessagePack, Interpret, Inject)
  deriving (JSONSchema, ToJSON, FromJSON) via GenericJSON UpstreamCfg

instance Default ControlCfg where
  def = ControlCfg
    { minimumControlInterval = 1 & seconds,
      minimumWaitInterval = 1 & seconds,
      staticPower = watts 200,
      speedThreshold = 1.1,
      learnCfg = Contextual (CtxCfg 4000),
      referenceMeasurementRoundInterval = unsafeRefine 6,
      hint = Full
    }

instance Default HwmonCfg where
  def = HwmonCfg
    { hwmonEnabled = True,
      hwmonPath = "/sys/class/hwmon"
    }

instance Default RaplCfg where
  def = RaplCfg
    { raplPath = "/sys/devices/virtual/powercap/intel-rapl",
      raplActions = watts <$> [100, 200],
      referencePower = watts 250
    }

instance Default DownstreamCfg where
  def = DownstreamCfg {downstreamBindAddress = "ipc:///tmp/nrm-downstream-event"}

instance Default Cfg where
  def = Cfg
    { logfile = "/tmp/nrm.log",
      hwloc = "hwloc",
      perf = "perf",
      argo_perf_wrapper = "nrm-perfwrapper",
      argo_nodeos_config = "argo_nodeos_config",
      libnrmPath = Nothing,
      pmpi_lib = "pmpi_lib",
      singularity = False,
      dummy = True,
      nodeos = False,
      slice_runtime = Dummy,
      downstreamCfg = def,
      upstreamCfg = def,
      raplCfg = Just def,
      hwmonCfg = def,
      verbose = NRM.Types.Configuration.Error,
      controlCfg = NoControl,
      passiveSensorFrequency = 1 & hz,
      extraStaticPassiveSensors = [],
      extraStaticActuators = []
    }

instance Default UpstreamCfg where
  def = UpstreamCfg
    { upstreamBindAddress = "*",
      pubPort = 2345,
      rpcPort = 3456
    }

jsonOptions :: Options
jsonOptions = defaultOptions {omitNothingFields = True}

examples :: Map Text Cfg
examples =
  [ ( "control",
      def
        { controlCfg = def
        }
    ),
    ( "extra-static-sensor",
      def
        { extraStaticPassiveSensors =
            [ ( "example extra static passive power sensor",
                ExtraPassiveSensor
                  { sensorBinary = "echo",
                    sensorArguments = ["30"],
                    sensorBehavior = IntervalBased,
                    range = 1 ... 40,
                    tags = [Power]
                  }
              )
            ]
        }
    ),
    ( "extra-static-actuator",
      def
        { extraStaticActuators =
            [ ( "example extra actuator",
                ExtraActuator
                  { actuatorBinary = "bash",
                    actuatorArguments = ["-c", "echo $@ >> /tmp/test-nrm-example-extra-actuator", "-o"],
                    actions = [DiscreteDouble 1, DiscreteDouble 2],
                    referenceAction = DiscreteDouble 1
                  }
              )
            ]
        }
    ),
    ( "variorum-two-package-power-limit-sensor",
      def {extraStaticPassiveSensors = [mkVariorumPowerLimitSensor 0, mkVariorumPowerLimitSensor 1]}
    ),
    ( "variorum-two-package-power-value-sensor",
      def {extraStaticPassiveSensors = [mkVariorumPowerSensor 0, mkVariorumPowerSensor 1]}
    ),
    ( "variorum-two-package-power-limits-actuator",
      def
        { extraStaticActuators =
            [ ( "example extra actuator",
                ExtraActuator
                  { actuatorBinary = "variorum-set-socket-power-limits-example",
                    actuatorArguments = [],
                    actions = [DiscreteDouble 100, DiscreteDouble 150],
                    referenceAction = DiscreteDouble 100
                  }
              )
            ]
        }
    )
  ]
  where
    mkVariorumPowerLimitSensor :: Int -> (Text, ExtraPassiveSensor)
    mkVariorumPowerLimitSensor x =
      ( "Sensor that gets package power limits for package " <> show x <> " through variorum",
        ExtraPassiveSensor
          { sensorBinary = "bash",
            sensorArguments = ["-c", "variorum-print-power-limits-example | awk '{ if ($1 == \"_PACKAGE_POWER_LIMITS\" && $2 == \"0x610\" && $4 == " <> show x <> " ) { print $6 } }'"],
            sensorBehavior = IntervalBased,
            range = 1 ... 40,
            tags = []
          }
      )
    mkVariorumPowerSensor :: Int -> (Text, ExtraPassiveSensor)
    mkVariorumPowerSensor x =
      ( "Sensor that gets package power limits for package " <> show x <> " through variorum",
        ExtraPassiveSensor
          { sensorBinary = "bash",
            sensorArguments = ["-c", "variorum-print-power-limits-example | awk '{ if ($1 == \"_PACKAGE_ENERGY_STATUS\" && $2 == \"0x610\" && $4 == " <> show x <> " ) { print $6 } }'"],
            sensorBehavior = Cumulative,
            range = 1 ... 40,
            tags = [Power]
          }
      )
