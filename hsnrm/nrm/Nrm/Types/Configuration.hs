{-|
Module      : Nrm.Types.Configuration
Copyright   : (c) UChicago Argonne, 2019
License     : BSD3
Maintainer  : fre@freux.fr
-}
module Nrm.Types.Configuration
  ( Cfg (..)
  , ContainerRuntime (..)
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
import Data.Flat
import Data.MessagePack
import Data.Yaml ()
import Data.Yaml.Internal ()
import Dhall
import Protolude

data ContainerRuntime = Singularity | Nodeos | Dummy
  deriving (Eq, Show, Generic, MessagePack, Interpret, Inject, Flat)

data DaemonVerbosity = Normal | Verbose | Debug
  deriving (Eq, Show, Generic, MessagePack, Interpret, Inject, Flat)

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
      , container_runtime :: ContainerRuntime
      , downstreamCfg :: DownstreamCfg
      , upstreamCfg :: UpstreamCfg
      , raplCfg :: RaplCfg
      , hwmonCfg :: HwmonCfg
      }
  deriving (Eq, Show, Generic, MessagePack, Flat)

data HwmonCfg
  = HwmonCfg
      { hwmonEnabled :: Bool
      , hwmonPath :: Text
      }
  deriving (Eq, Show, Generic, MessagePack, Interpret, Inject, Flat)

data RaplCfg
  = RaplCfg
      { raplEnabled :: Bool
      , raplPath :: Text
      }
  deriving (Eq, Show, Generic, MessagePack, Interpret, Inject, Flat)

newtype DownstreamCfg
  = DownstreamCfg
      { downstreamBindAddress :: Text
      }
  deriving (Eq, Show, Generic, MessagePack, Interpret, Inject, Flat)

data UpstreamCfg
  = UpstreamCfg
      { upstreamBindAddress :: Text
      , pubPort :: Int
      , rpcPort :: Int
      }
  deriving (Eq, Show, Generic, MessagePack, Flat)

instance Default HwmonCfg where

  def = HwmonCfg
    { hwmonEnabled = True
    , hwmonPath = "/sys/devices/virtual/powercap/intel-rapl"
    }

instance Default RaplCfg where

  def = RaplCfg
    { raplEnabled = True
    , raplPath = "/sys/devices/virtual/powercap/intel-rapl"
    }

instance ToJSON ContainerRuntime where

  toJSON = genericToJSON jsonOptions

  toEncoding = genericToEncoding jsonOptions

instance ToJSON DaemonVerbosity where

  toJSON = genericToJSON jsonOptions

  toEncoding = genericToEncoding jsonOptions

instance ToJSON HwmonCfg where

  toJSON = genericToJSON jsonOptions

  toEncoding = genericToEncoding jsonOptions

instance ToJSON RaplCfg where

  toJSON = genericToJSON jsonOptions

  toEncoding = genericToEncoding jsonOptions

instance ToJSON DownstreamCfg where

  toJSON = genericToJSON jsonOptions

  toEncoding = genericToEncoding jsonOptions

instance FromJSON ContainerRuntime where

  parseJSON = genericParseJSON jsonOptions

instance FromJSON DaemonVerbosity where

  parseJSON = genericParseJSON jsonOptions

instance FromJSON HwmonCfg where

  parseJSON = genericParseJSON jsonOptions

instance FromJSON RaplCfg where

  parseJSON = genericParseJSON jsonOptions

instance FromJSON DownstreamCfg where

  parseJSON = genericParseJSON jsonOptions

instance Default DownstreamCfg where

  def = DownstreamCfg {downstreamBindAddress = "ipc:///tmp/nrm-downstream-event"}

jsonOptions :: Options
jsonOptions = defaultOptions {omitNothingFields = True}
