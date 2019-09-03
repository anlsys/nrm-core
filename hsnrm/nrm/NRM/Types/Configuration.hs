{-# LANGUAGE DerivingVia #-}

{-|
Module      : NRM.Types.Configuration
Copyright   : (c) UChicago Argonne, 2019
License     : BSD3
Maintainer  : fre@freux.fr
-}
module NRM.Types.Configuration
  ( Cfg (..)
  , SliceRuntime (..)
  , UpstreamCfg (..)
  , DownstreamCfg (..)
  , DaemonVerbosity (..)
  , RaplCfg (..)
  , HwmonCfg (..)
  , jsonOptions
  )
where

import Data.Aeson
import Data.Default
import Data.JSON.Schema
import Data.MessagePack
import Data.Yaml.Internal ()
{-import NRM.Types.Units-}
import Dhall
import NRM.Classes.Messaging
import Protolude

data SliceRuntime = Singularity | Nodeos | Dummy
  deriving (Eq, Show, Generic, MessagePack, Interpret, Inject)
  deriving (JSONSchema, ToJSON, FromJSON) via GenericJSON SliceRuntime

data DaemonVerbosity = Normal | Verbose | Debug
  deriving (Eq, Show, Generic, MessagePack, Interpret, Inject)
  deriving (JSONSchema, ToJSON, FromJSON) via GenericJSON DaemonVerbosity

data Cfg
  = Cfg
      { verbose :: DaemonVerbosity
      , logfile :: Text
      , hwloc :: Text
      , perf :: Text
      , argo_perf_wrapper :: Text
      , argo_nodeos_config :: Text
      , pmpi_lib :: Text
      , singularity :: Bool
      , dummy :: Bool
      , nodeos :: Bool
      , slice_runtime :: SliceRuntime
      , downstreamCfg :: DownstreamCfg
      , upstreamCfg :: UpstreamCfg
      , raplCfg :: RaplCfg
      , hwmonCfg :: HwmonCfg
      }
  deriving (Eq, Show, Generic, MessagePack)
  deriving (JSONSchema, ToJSON, FromJSON) via GenericJSON Cfg

data HwmonCfg
  = HwmonCfg
      { hwmonEnabled :: Bool
      , hwmonPath :: Text
      }
  deriving (Eq, Show, Generic, MessagePack, Interpret, Inject)
  deriving (JSONSchema, ToJSON, FromJSON) via GenericJSON HwmonCfg

data RaplCfg
  = RaplCfg
      { raplEnabled :: Bool
      , raplPath :: Text
      {-, raplFrequency :: Integer-}
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
      { upstreamBindAddress :: Text
      , pubPort :: Int
      , rpcPort :: Int
      }
  deriving (Eq, Show, Generic, MessagePack)
  deriving (JSONSchema, ToJSON, FromJSON) via GenericJSON UpstreamCfg

instance Default HwmonCfg where

  def = HwmonCfg
    { hwmonEnabled = True
    , hwmonPath = "/sys/class/hwmon"
    }

instance Default RaplCfg where

  def = RaplCfg
    { raplEnabled = True
    , raplPath = "/sys/devices/virtual/powercap/intel-rapl"
    }

instance Default DownstreamCfg where

  def = DownstreamCfg {downstreamBindAddress = "ipc:///tmp/nrm-downstream-event"}

jsonOptions :: Options
jsonOptions = defaultOptions {omitNothingFields = True}
