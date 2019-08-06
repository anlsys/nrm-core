{-|
Module      : Nrm.Types.Configuration.Internal
Description : Nrm configuration
Copyright   : (c) UChicago Argonne, 2019
License     : BSD3
Maintainer  : fre@freux.fr
-}
module Nrm.Types.Configuration.Internal
  ( Cfg (..)
  , ContainerRuntime (..)
  , UpstreamCfg (..)
  , DownstreamCfg (..)
  , DaemonVerbosity (..)
  )
where

import Data.Aeson
import Data.Default
import Data.Flat
import Data.Yaml ()
import Data.Yaml.Internal ()
import Dhall
import Protolude

data ContainerRuntime = Singularity | Nodeos | Dummy
  deriving (Generic, ToJSON, Interpret, Flat)

data DaemonVerbosity = Normal | Verbose
  deriving (Eq, Generic, ToJSON, Interpret, Flat)

data Cfg
  = Cfg
      { verbose :: DaemonVerbosity
      , logfile :: Text
      , hwloc :: Text
      , perf :: Text
      , argo_perf_wrapper :: Text
      , argo_nodeos_config :: Text
      , pmpi_lib :: Text
      , singularity :: Text
      , container_runtime :: ContainerRuntime
      , downstreamCfg :: DownstreamCfg
      , upstreamCfg :: UpstreamCfg
      }
  deriving (Generic, Interpret, Flat)

newtype DownstreamCfg
  = DownstreamCfg
      { downstreamBindAddress :: Text
      }
  deriving (Generic, Interpret, Flat, ToJSON)

data UpstreamCfg
  = UpstreamCfg
      { upstreamBindAddress :: Text
      , pubPort :: Integer
      , rpcPort :: Integer
      }
  deriving (Generic, Interpret, Flat, ToJSON)

instance FromJSON ContainerRuntime where

  parseJSON = genericParseJSON defaultOptions {omitNothingFields = True}

instance FromJSON DaemonVerbosity where

  parseJSON = genericParseJSON defaultOptions {omitNothingFields = True}

instance FromJSON DownstreamCfg where

  parseJSON = genericParseJSON defaultOptions {omitNothingFields = True}

instance FromJSON UpstreamCfg where

  parseJSON = genericParseJSON defaultOptions {omitNothingFields = True}

instance Default UpstreamCfg where

  def = UpstreamCfg
    { upstreamBindAddress = "*"
    , pubPort = 2345
    , rpcPort = 3456
    }

instance Default DownstreamCfg where

  def = DownstreamCfg {downstreamBindAddress = "ipc:///tmp/nrm-downstream-event"}

instance Default Cfg where

  def = Cfg
    { logfile = "/tmp/nrm.log"
    , hwloc = "hwloc"
    , perf = "perf"
    , argo_perf_wrapper = "nrm-perfwrapper"
    , argo_nodeos_config = "argo_nodeos_config"
    , pmpi_lib = "pmpi_lib"
    , singularity = "singularity"
    , container_runtime = Dummy
    , downstreamCfg = def
    , upstreamCfg = def
    , verbose = Verbose
    }
