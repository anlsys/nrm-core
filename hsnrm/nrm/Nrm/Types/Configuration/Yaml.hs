{-|
Module      : Nrm.Types.Configuration.Yaml
Copyright   : (c) UChicago Argonne, 2019
License     : BSD3
Maintainer  : fre@freux.fr
-}
module Nrm.Types.Configuration.Yaml
  ( decodeCfgFile
  , decodeCfg
  , encodeCfg
  , encodeDCfg
  )
where

import Data.Aeson
import Data.Default
import Data.Yaml
import qualified Nrm.Types.Configuration as I
import qualified Nrm.Types.Configuration.Dhall as D
import Protolude
import System.IO.Error

data Cfg
  = Cfg
      { verbose :: Maybe Bool
      , logfile :: Maybe Text
      , hwloc :: Maybe Text
      , perf :: Maybe Text
      , argo_perf_wrapper :: Maybe Text
      , argo_nodeos_config :: Maybe Text
      , pmpi_lib :: Maybe Text
      , singularity :: Maybe Bool
      , dummy :: Maybe Bool
      , nodeos :: Maybe Bool
      , container_runtime :: Maybe D.ContainerRuntime
      , downstreamCfg :: Maybe D.DownstreamCfg
      , upstreamCfg :: Maybe D.UpstreamCfg
      , raplCfg :: Maybe D.RaplCfg
      , hwmonCfg :: Maybe D.HwmonCfg
      }
  deriving (Generic)

instance ToJSON Cfg where

  toJSON = genericToJSON I.jsonOptions

  toEncoding = genericToEncoding I.jsonOptions

instance FromJSON Cfg where

  parseJSON = genericParseJSON I.jsonOptions

toInternal :: Cfg -> D.Cfg
toInternal d = D.Cfg
  { verbose = if verbose d == Just True then D.Verbose else D.Normal
  , logfile = fromDefault logfile D.logfile
  , hwloc = fromDefault hwloc D.hwloc
  , perf = fromDefault perf D.perf
  , argo_perf_wrapper = fromDefault argo_perf_wrapper D.argo_perf_wrapper
  , argo_nodeos_config = fromDefault argo_nodeos_config D.argo_nodeos_config
  , pmpi_lib = fromDefault pmpi_lib D.pmpi_lib
  , singularity = fromDefault singularity D.singularity
  , dummy = fromDefault dummy D.dummy
  , nodeos = fromDefault nodeos D.nodeos
  , container_runtime = fromDefault container_runtime D.container_runtime
  , downstreamCfg = fromDefault downstreamCfg D.downstreamCfg
  , upstreamCfg = fromDefault upstreamCfg D.upstreamCfg
  , raplCfg = fromDefault raplCfg D.raplCfg
  , hwmonCfg = fromDefault hwmonCfg D.hwmonCfg
  }
  where
    fromDefault :: Default a => (Cfg -> Maybe c) -> (a -> c) -> c
    fromDefault attr attd = fromMaybe (attd def) (attr d)

fromInternal :: D.Cfg -> Cfg
fromInternal d = Cfg
  { verbose = if D.verbose d == D.verbose (def :: D.Cfg) then Just True else Nothing
  , logfile = toJust D.logfile
  , hwloc = toJust D.hwloc
  , perf = toJust D.perf
  , argo_perf_wrapper = toJust D.argo_perf_wrapper
  , argo_nodeos_config = toJust D.argo_nodeos_config
  , pmpi_lib = toJust D.pmpi_lib
  , singularity = toJust D.singularity
  , dummy = toJust D.dummy
  , nodeos = toJust D.nodeos
  , container_runtime = toJust D.container_runtime
  , downstreamCfg = toJust D.downstreamCfg
  , upstreamCfg = toJust D.upstreamCfg
  , raplCfg = toJust D.raplCfg
  , hwmonCfg = toJust D.hwmonCfg
  }
  where
    toJust :: (Eq a) => (D.Cfg -> a) -> Maybe a
    toJust x = if x (def :: D.Cfg) == xd then Nothing else Just xd
      where
        xd = x d

decodeCfgFile :: (MonadIO m) => Text -> m I.Cfg
decodeCfgFile fn =
  liftIO $ try (decodeFileEither (toS fn)) >>= \case
    Left e -> throwError e
    Right (Left pa) -> throwError $ userError $ "parse fail:" <> show pa
    Right (Right a) -> return $ D.toInternal $ toInternal a

decodeCfg :: ByteString -> Either ParseException I.Cfg
decodeCfg fn = D.toInternal . toInternal <$> decodeEither' fn

encodeCfg :: I.Cfg -> ByteString
encodeCfg = Data.Yaml.encode . Data.Aeson.toJSON . fromInternal . D.fromInternal

encodeDCfg :: D.Cfg -> ByteString
encodeDCfg = Data.Yaml.encode . Data.Aeson.toJSON . fromInternal
