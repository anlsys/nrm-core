{-# LANGUAGE DerivingVia #-}

{-|
Module      : Nrm.Types.Configuration
Description : Nrm configuration
Copyright   : (c) UChicago Argonne, 2019
License     : BSD3
Maintainer  : fre@freux.fr
-}
module Nrm.Types.Configuration
  ( Cfg (..)
  , inputCfg
  , inputCfgIOFlat
  )
where

import Data.Default
import Data.Flat
import Data.Flat.Run
import Dhall
import Protolude

data ContainerRuntime = Singularity | Nodeos | Dummy
  deriving (Generic, Interpret, Flat)

{-deriving via Int instance MessagePack Integer-}
data Cfg
  = Cfg
      { logfile :: Text
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
  deriving (Generic, Interpret, Flat)

-- TODO use Network.Socket.PortNumber
--
data UpstreamCfg = UpstreamCfg {upstreamBindAddress :: Text, pubPort :: Integer, rpcPort :: Integer}
  deriving (Generic, Interpret, Flat)

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
    }

inputCfg :: (MonadIO m) => Text -> m Cfg
inputCfg fn =
  liftIO $ try (input ft fn) >>= \case
    Right d -> return d
    Left e -> throwError e
  where
    ft :: Dhall.Type Cfg
    ft = Dhall.auto

inputCfgIOFlat :: Text -> IO ByteString
inputCfgIOFlat = undefined
