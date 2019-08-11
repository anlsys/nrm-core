{-|
Module      : Nrm.Types.Configuration.Dhall
Copyright   : (c) UChicago Argonne, 2019
License     : BSD3
Maintainer  : fre@freux.fr
-}
module Nrm.Types.Configuration.Dhall
  ( inputCfg
  , toInternal
  , fromInternal
  , Cfg (..)
  , I.DaemonVerbosity (..)
  , I.ContainerRuntime (..)
  , UpstreamCfg (..)
  , I.DownstreamCfg (..)
  )
where

import Data.Aeson
import Data.Default
import Dhall
import qualified Nrm.Types.Configuration as I
import Protolude

-- As soon as Internal.Cfg isn't Interpretable, we write a dhall
-- interpretable datatype layer here. As it stands, this is a transitive
-- "identity" placeholder.
data Cfg
  = Cfg
      { verbose :: I.DaemonVerbosity
      , logfile :: Text
      , hwloc :: Text
      , perf :: Text
      , argo_perf_wrapper :: Text
      , argo_nodeos_config :: Text
      , pmpi_lib :: Text
      , singularity :: Text
      , container_runtime :: I.ContainerRuntime
      , downstreamCfg :: I.DownstreamCfg
      , upstreamCfg :: UpstreamCfg
      }
  deriving (Eq, Show, Generic, Interpret, Inject)

data UpstreamCfg
  = UpstreamCfg
      { upstreamBindAddress :: Text
      , pubPort :: Integer
      , rpcPort :: Integer
      }
  deriving (Eq, Show, Generic, Interpret, Inject)

instance Default Cfg where

  def = Cfg
    { logfile = "/tmp/nrm.log"
    , hwloc = "hwloc"
    , perf = "perf"
    , argo_perf_wrapper = "nrm-perfwrapper"
    , argo_nodeos_config = "argo_nodeos_config"
    , pmpi_lib = "pmpi_lib"
    , singularity = "singularity"
    , container_runtime = I.Dummy
    , downstreamCfg = def
    , upstreamCfg = def
    , verbose = I.Normal
    }

instance Default UpstreamCfg where

  def = UpstreamCfg
    { upstreamBindAddress = "*"
    , pubPort = 2345
    , rpcPort = 3456
    }

instance FromJSON Cfg where

  parseJSON = genericParseJSON I.jsonOptions

instance ToJSON Cfg where

  toJSON = genericToJSON I.jsonOptions

  toEncoding = genericToEncoding I.jsonOptions

instance FromJSON UpstreamCfg where

  parseJSON = genericParseJSON I.jsonOptions

instance ToJSON UpstreamCfg where

  toJSON = genericToJSON I.jsonOptions

  toEncoding = genericToEncoding I.jsonOptions

inputDCfg :: (MonadIO m) => Text -> m Cfg
inputDCfg fn =
  liftIO $ try (input (Dhall.auto :: Dhall.Type Cfg) fn) >>= \case
    Right d -> return d
    Left e -> throwError e

toInternal :: Cfg -> I.Cfg
toInternal Cfg {..} = I.Cfg {upstreamCfg = toInternalUpstreamCfg upstreamCfg, ..}

toInternalUpstreamCfg :: UpstreamCfg -> I.UpstreamCfg
toInternalUpstreamCfg UpstreamCfg {..} = I.UpstreamCfg
  { pubPort = fromInteger pubPort
  , rpcPort = fromInteger rpcPort
  , ..
  }

fromInternal :: I.Cfg -> Cfg
fromInternal I.Cfg {..} = Cfg {upstreamCfg = fromInternalUpstreamCfg upstreamCfg, ..}

fromInternalUpstreamCfg :: I.UpstreamCfg -> UpstreamCfg
fromInternalUpstreamCfg I.UpstreamCfg {..} = UpstreamCfg
  { pubPort = toInteger pubPort
  , rpcPort = toInteger rpcPort
  , ..
  }

inputCfg :: (MonadIO m) => Text -> m I.Cfg
inputCfg fn = toInternal <$> inputDCfg fn
