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
    jsonOptions,
    examples,
  )
where

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
import NRM.Types.Units
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
        activeSensorFrequency :: Frequency
      }
  deriving (Eq, Show, Generic, MessagePack, Interpret, Inject)
  deriving (JSONSchema, ToJSON, FromJSON) via GenericJSON Cfg

data ControlCfg
  = ControlCfg
      { minimumControlInterval :: Time,
        staticPower :: Power,
        learnCfg :: LearnConfig,
        speedThreshold :: Double,
        referenceMeasurementRoundInterval :: Refined (GreaterThan 5) Int,
        hint :: Hint
      }
  | FixedCommand
      { fixedPower :: Power
      }
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
    { minimumControlInterval = 0.1 & seconds,
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
      controlCfg = FixedCommand (watts 250),
      activeSensorFrequency = 1 & hz
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
  [ ("default", def),
    ( "control",
      def
        { controlCfg = def
        }
    )
  ]
