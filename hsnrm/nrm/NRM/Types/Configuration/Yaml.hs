{-|
Module      : NRM.Types.Configuration.Yaml
Copyright   : (c) UChicago Argonne, 2019
License     : BSD3
Maintainer  : fre@freux.fr
-}
module NRM.Types.Configuration.Yaml
  ( decodeCfgFile
  , decodeCfg
  , encodeCfg
  , encodeDCfg
  )
where

import Data.Aeson
import Data.Default
import Data.Yaml
import qualified NRM.Types.Configuration as I
import qualified NRM.Types.Process as Process
import Protolude
import System.IO.Error

data Cfg
  = Cfg
      { verbose :: Maybe Bool
      , logfile :: Maybe Text
      , hwloc :: Maybe Text
      , perf :: Maybe Text
      , argo_perf_wrapper :: Maybe Process.Command
      , argo_nodeos_config :: Maybe Process.Command
      , pmpi_lib :: Maybe Text
      , singularity :: Maybe Bool
      , dummy :: Maybe Bool
      , nodeos :: Maybe Bool
      , slice_runtime :: Maybe I.SliceRuntime
      , downstreamCfg :: Maybe I.DownstreamCfg
      , upstreamCfg :: Maybe I.UpstreamCfg
      , raplCfg :: Maybe I.RaplCfg
      , hwmonCfg :: Maybe I.HwmonCfg
      }
  deriving (Generic)

instance ToJSON Cfg where

  toJSON = genericToJSON I.jsonOptions

  toEncoding = genericToEncoding I.jsonOptions

instance FromJSON Cfg where

  parseJSON = genericParseJSON I.jsonOptions

toInternal :: Cfg -> I.Cfg
toInternal d = I.Cfg
  { verbose = if verbose d == Just True then I.Verbose else I.Normal
  , logfile = fromDefault logfile I.logfile
  , hwloc = fromDefault hwloc I.hwloc
  , perf = fromDefault perf I.perf
  , argo_perf_wrapper = fromDefault argo_perf_wrapper I.argo_perf_wrapper
  , argo_nodeos_config = fromDefault argo_nodeos_config I.argo_nodeos_config
  , pmpi_lib = fromDefault pmpi_lib I.pmpi_lib
  , singularity = fromDefault singularity I.singularity
  , dummy = fromDefault dummy I.dummy
  , nodeos = fromDefault nodeos I.nodeos
  , slice_runtime = fromDefault slice_runtime I.slice_runtime
  , downstreamCfg = fromDefault downstreamCfg I.downstreamCfg
  , upstreamCfg = fromDefault upstreamCfg I.upstreamCfg
  , raplCfg = fromDefault raplCfg I.raplCfg
  , hwmonCfg = fromDefault hwmonCfg I.hwmonCfg
  }
  where
    fromDefault :: Default a => (Cfg -> Maybe c) -> (a -> c) -> c
    fromDefault attr attd = fromMaybe (attd def) (attr d)

fromInternal :: I.Cfg -> Cfg
fromInternal d = Cfg
  { verbose = if I.verbose d == I.verbose (def :: I.Cfg) then Just True else Nothing
  , logfile = toJust I.logfile
  , hwloc = toJust I.hwloc
  , perf = toJust I.perf
  , argo_perf_wrapper = toJust I.argo_perf_wrapper
  , argo_nodeos_config = toJust I.argo_nodeos_config
  , pmpi_lib = toJust I.pmpi_lib
  , singularity = toJust I.singularity
  , dummy = toJust I.dummy
  , nodeos = toJust I.nodeos
  , slice_runtime = toJust I.slice_runtime
  , downstreamCfg = toJust I.downstreamCfg
  , upstreamCfg = toJust I.upstreamCfg
  , raplCfg = toJust I.raplCfg
  , hwmonCfg = toJust I.hwmonCfg
  }
  where
    toJust :: (Eq a) => (I.Cfg -> a) -> Maybe a
    toJust x = if x (def :: I.Cfg) == xd then Nothing else Just xd
      where
        xd = x d

decodeCfgFile :: (MonadIO m) => Text -> m I.Cfg
decodeCfgFile fn =
  liftIO $ try (decodeFileEither (toS fn)) >>= \case
    Left e -> throwError e
    Right (Left pa) -> throwError $ userError $ "parse fail:" <> show pa
    Right (Right a) -> return $ toInternal a

decodeCfg :: ByteString -> Either ParseException I.Cfg
decodeCfg fn = toInternal <$> decodeEither' fn

encodeCfg :: I.Cfg -> ByteString
encodeCfg = Data.Yaml.encode . Data.Aeson.toJSON . fromInternal

encodeDCfg :: I.Cfg -> ByteString
encodeDCfg = Data.Yaml.encode . Data.Aeson.toJSON . fromInternal
