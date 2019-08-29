{-|
Module      : NRM.Types.Configuration.Dhall
Copyright   : (c) UChicago Argonne, 2019
License     : BSD3
Maintainer  : fre@freux.fr
-}
module NRM.Types.Configuration.Dhall
  ( inputCfg
  , toInternal
  , fromInternal
  , Cfg (..)
  , I.DaemonVerbosity (..)
  , I.SliceRuntime (..)
  , UpstreamCfg (..)
  , I.DownstreamCfg (..)
  , I.RaplCfg (..)
  , I.HwmonCfg (..)
  )
where

import Data.Aeson
import Data.Default
import Dhall
import qualified NRM.Types.Configuration as I
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
      , singularity :: Bool
      , dummy :: Bool
      , nodeos :: Bool
      , slice_runtime :: I.SliceRuntime
      , downstreamCfg :: I.DownstreamCfg
      , upstreamCfg :: UpstreamCfg
      , raplCfg :: I.RaplCfg
      , hwmonCfg :: I.HwmonCfg
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
    , singularity = False
    , dummy = True
    , nodeos = False
    , slice_runtime = I.Dummy
    , downstreamCfg = def
    , upstreamCfg = def
    , raplCfg = def
    , hwmonCfg = def
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
