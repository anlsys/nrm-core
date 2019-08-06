{-|
Module      : Nrm.Types.Manifest.Dhall
Description : Nrm application manifest dhall reader
Copyright   : (c) UChicago Argonne, 2019
License     : BSD3
Maintainer  : fre@freux.fr
-}
module Nrm.Types.Manifest.Dhall
  ( inputManifest
  , toInternal
  , fromInternal
  , I.Manifest(..)
  )
where

import Dhall
import qualified Nrm.Types.Manifest.Internal as I
import Protolude

-- TODO : As soon as Internal.Cfg isn't Interpretable,
-- we write a dhall interpretable layer here. As it stands, this is essentially
-- a transitive "identity" placeholder

inputDManifest :: (MonadIO m) => Text -> m I.Manifest
inputDManifest fn =
  liftIO $ try (input dt fn) >>= \case
    Right d -> return d
    Left e -> throwError e
  where
    dt :: Dhall.Type I.Manifest
    dt = Dhall.auto

toInternal :: I.Manifest -> I.Manifest
toInternal I.Manifest {..} = undefined

fromInternal :: I.Manifest -> I.Manifest
fromInternal I.Manifest {..} = undefined

inputManifest :: (MonadIO m) => Text -> m I.Manifest
inputManifest fn = toInternal <$> inputDManifest fn
