{-|
Module      : Nrm.Types.Configuration.Dhall
Description : Nrm configuration dhall reader
Copyright   : (c) UChicago Argonne, 2019
License     : BSD3
Maintainer  : fre@freux.fr
-}
module Nrm.Types.Configuration.Dhall
  ( inputCfg
  , toInternal
  , fromInternal
  , I.Cfg (..)
  )
where

import Dhall
import qualified Nrm.Types.Configuration.Internal as I
import Protolude

-- TODO : As soon as Internal.Cfg isn't Interpretable,
-- we write a dhall interpretable layer here. As it stands, this is essentially
-- a transitive "identity" placeholder

{-data Cfg-}
{-= Cfg-}
{-{ verbose :: I.Verbosity-}
{-, logfile :: Text-}
{-, hwloc :: Text-}
{-, perf :: Text-}
{-, argo_perf_wrapper :: Text-}
{-, argo_nodeos_config :: Text-}
{-, pmpi_lib :: Text-}
{-, singularity :: Text-}
{-, container_runtime :: I.ContainerRuntime-}
{-, downstreamCfg :: I.DownstreamCfg-}
{-, upstreamCfg :: I.UpstreamCfg-}
{-}-}
{-deriving (Generic, Interpret)-}
inputDCfg :: (MonadIO m) => Text -> m I.Cfg
inputDCfg fn =
  liftIO $ try (input dt fn) >>= \case
    Right d -> return d
    Left e -> throwError e
  where
    dt :: Dhall.Type I.Cfg
    dt = Dhall.auto

toInternal :: I.Cfg -> I.Cfg
toInternal I.Cfg {..} = undefined

fromInternal :: I.Cfg -> I.Cfg
fromInternal I.Cfg {..} = undefined

inputCfg :: (MonadIO m) => Text -> m I.Cfg
inputCfg fn = toInternal <$> inputDCfg fn
