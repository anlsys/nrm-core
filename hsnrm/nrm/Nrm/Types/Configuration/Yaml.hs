{-|
Module      : Nrm.Types.Configuration.Yaml
Description : Nrm configuration yaml reader
Copyright   : (c) UChicago Argonne, 2019
License     : BSD3
Maintainer  : fre@freux.fr
-}
module Nrm.Types.Configuration.Yaml
  ( Cfg (..)
  , ContainerRuntime (..)
  , Verbosity (..)
  , decodeCfgFile
  , decodeCfg
  , encodeCfg
  )
where

import Data.Aeson
import Data.Yaml
import Dhall
import qualified Nrm.Types.Configuration.Dhall as D
import qualified Nrm.Types.Configuration.Internal as I
import Protolude
import System.IO.Error

data ContainerRuntime = Singularity | Nodeos | Dummy
  deriving (Generic, Interpret, ToJSON)

instance FromJSON ContainerRuntime where

  parseJSON = genericParseJSON defaultOptions {omitNothingFields = True}

data Verbosity = Normal | Verbose
  deriving (Generic, Interpret, ToJSON)

instance FromJSON Verbosity where

  parseJSON = genericParseJSON defaultOptions {omitNothingFields = True}

data Cfg
  = Cfg
      { verbose :: Maybe Verbosity
      , logfile :: Maybe Text
      , hwloc :: Maybe Text
      , perf :: Maybe Text
      , argo_perf_wrapper :: Maybe Text
      , argo_nodeos_config :: Maybe Text
      , pmpi_lib :: Maybe Text
      , singularity :: Maybe Text
      , container_runtime :: Maybe ContainerRuntime
      , downstreamCfg :: Maybe DownstreamCfg
      , upstreamCfg :: Maybe UpstreamCfg
      }
  deriving (Generic, Interpret, ToJSON)

instance FromJSON Cfg where

  parseJSON = genericParseJSON defaultOptions {omitNothingFields = True}

newtype DownstreamCfg
  = DownstreamCfg
      { downstreamBindAddress :: Text
      }
  deriving (Generic, Interpret, ToJSON)

instance FromJSON DownstreamCfg where

  parseJSON = genericParseJSON defaultOptions {omitNothingFields = True}

data UpstreamCfg
  = UpstreamCfg
      { upstreamBindAddress :: Text
      , pubPort :: Integer
      , rpcPort :: Integer
      }
  deriving (Generic, Interpret, ToJSON)

instance FromJSON UpstreamCfg where

  parseJSON = genericParseJSON defaultOptions {omitNothingFields = True}

toInternal :: Cfg -> D.Cfg
toInternal d = undefined

{-D.Cfg-}
{-{ cmds     = toInternalCmd <$> cmds d-}
{-, verbose  = fromMaybe False (verbose d)-}
{-, cleaning = fromMaybe False (cleaning d)-}
{-, pre      = fromMaybe [] (pre d)-}
{-, post     = fromMaybe [] (post d)-}
{-, workdir  = fromMaybe "./" (workdir d)-}
{-}-}
fromInternal :: D.Cfg -> Cfg
fromInternal d = undefined

{-fromInternal d = Cfg {..}-}
{-where-}
{-workdir = case D.workdir d of-}
{-"./" -> Nothing-}
{-w    -> Just w-}
{-cmds     = fromInternalCmd <$> D.cmds d-}
{-verbose  = if D.verbose d then Just True else Nothing-}
{-cleaning = if D.cleaning d then Just True else Nothing-}
{-pre      = case D.pre d of-}
{-[] -> Nothing-}
{-l  -> Just l-}
{-post = case D.post d of-}
{-[] -> Nothing-}
{-l  -> Just l-}

decodeCfgFile :: (MonadIO m) => Text -> m I.Cfg
decodeCfgFile fn =
  liftIO $ try (decodeFileEither (toS fn)) >>= \case
    Left e -> throwError e
    Right (Left pa) -> throwError $ userError $ "parse fail:" <> show pa
    Right (Right a) -> return $ D.toInternal $ toInternal a

decodeCfg :: ByteString -> Either ParseException I.Cfg
decodeCfg fn = D.toInternal . toInternal <$> decodeEither' fn

encodeCfg :: I.Cfg -> ByteString
encodeCfg = Data.Yaml.encode . fromInternal . D.fromInternal
