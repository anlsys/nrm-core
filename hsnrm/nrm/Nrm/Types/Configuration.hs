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
  deriving (Eq, Show, Generic, Interpret, Inject, Flat)

data DaemonVerbosity = Normal | Verbose | Debug
  deriving (Eq, Show, Generic, Interpret, Inject, Flat)

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
      }
  deriving (Eq, Show, Generic, Flat)

newtype DownstreamCfg
  = DownstreamCfg
      { downstreamBindAddress :: Text
      }
  deriving (Eq, Show, Generic, Interpret, Inject, Flat)

data UpstreamCfg
  = UpstreamCfg
      { upstreamBindAddress :: Text
      , pubPort :: Int
      , rpcPort :: Int
      }
  deriving (Eq, Show, Generic, Flat)

deriving instance MessagePack Cfg

deriving instance MessagePack DaemonVerbosity

deriving instance MessagePack UpstreamCfg

deriving instance MessagePack DownstreamCfg

deriving instance MessagePack ContainerRuntime

{-toObject = toObject . flat-}

{-fromObject o = fromObject o >>= go-}
{-where-}
{-go :: (Monad m) => ByteString -> m Cfg-}
{-go bs = return (fromRight (panic "couldn't decode cfg from flat") (unflat bs))-}
instance ToJSON ContainerRuntime where

  toJSON = genericToJSON jsonOptions

  toEncoding = genericToEncoding jsonOptions

instance ToJSON DaemonVerbosity where

  toJSON = genericToJSON jsonOptions

  toEncoding = genericToEncoding jsonOptions

instance ToJSON DownstreamCfg where

  toJSON = genericToJSON jsonOptions

  toEncoding = genericToEncoding jsonOptions

instance FromJSON ContainerRuntime where

  parseJSON = genericParseJSON jsonOptions

instance FromJSON DaemonVerbosity where

  parseJSON = genericParseJSON jsonOptions

instance FromJSON DownstreamCfg where

  parseJSON = genericParseJSON jsonOptions

instance Default DownstreamCfg where

  def = DownstreamCfg {downstreamBindAddress = "ipc:///tmp/nrm-downstream-event"}

jsonOptions :: Options
jsonOptions = defaultOptions {omitNothingFields = True}
