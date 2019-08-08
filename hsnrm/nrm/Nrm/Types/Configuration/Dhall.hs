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
  , I.Cfg (..)
  , I.DaemonVerbosity (..)
  , I.ContainerRuntime (..)
  , I.UpstreamCfg (..)
  , I.DownstreamCfg (..)
  )
where

import Dhall
import qualified Nrm.Types.Configuration as I
import Protolude

-- As soon as Internal.Cfg isn't Interpretable, we write a dhall
-- interpretable datatype layer here. As it stands, this is a transitive
-- "identity" placeholder.
inputDCfg :: (MonadIO m) => Text -> m I.Cfg
inputDCfg fn =
  liftIO $ try (input (Dhall.auto :: Dhall.Type I.Cfg) fn) >>= \case
    Right d -> return d
    Left e -> throwError e

toInternal :: I.Cfg -> I.Cfg
toInternal = identity

fromInternal :: I.Cfg -> I.Cfg
fromInternal = identity

inputCfg :: (MonadIO m) => Text -> m I.Cfg
inputCfg fn = toInternal <$> inputDCfg fn
