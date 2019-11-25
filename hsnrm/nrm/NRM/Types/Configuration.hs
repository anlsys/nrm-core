{-# LANGUAGE DerivingVia #-}
{-# OPTIONS_GHC -fno-warn-incomplete-patterns #-}

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
    inputCfg,
  )
where

import Data.Aeson
import Data.Default
import Data.JSON.Schema
import Data.MessagePack
import Data.Yaml.Internal ()
import Dhall
import HBandit.Types
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
        controlCfg :: Maybe ControlCfg
      }
  deriving (Eq, Show, Generic, MessagePack, Interpret, Inject)
  deriving (JSONSchema, ToJSON, FromJSON) via GenericJSON Cfg

data ControlCfg
  = ControlCfg
      { minimumControlInterval :: Time,
        learnCfg :: LearnConfig,
        speedThreshold :: ZeroOne Double,
        referenceMeasurementRoundInterval :: Refined (GreaterThan 5) Int
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
        raplFrequency :: Frequency
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
      speedThreshold = fe (refine 0.9),
      learnCfg = LagrangeConstraints (LagrangeMultiplier 0.5),
      referenceMeasurementRoundInterval = unsafeRefine 6
    }
    where
      fe :: Either e (ZeroOne Double) -> ZeroOne Double
      fe (Right e) = e

instance Default HwmonCfg where
  def = HwmonCfg
    { hwmonEnabled = True,
      hwmonPath = "/sys/class/hwmon"
    }

instance Default RaplCfg where
  def = RaplCfg
    { raplPath = "/sys/devices/virtual/powercap/intel-rapl",
      raplFrequency = 1 & hz
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
      raplCfg = def,
      hwmonCfg = def,
      verbose = NRM.Types.Configuration.Error,
      controlCfg = Nothing
    }

instance Default UpstreamCfg where
  def = UpstreamCfg
    { upstreamBindAddress = "*",
      pubPort = 2345,
      rpcPort = 3456
    }

jsonOptions :: Options
jsonOptions = defaultOptions {omitNothingFields = True}

inputCfg :: (MonadIO m) => Text -> m Cfg
inputCfg fn =
  liftIO $
    try (input (Dhall.auto :: Dhall.Type Cfg) fn) >>= \case
      Right d -> return d
      Left e -> throwError e
