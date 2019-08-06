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
  , I.Manifest (..)
  , I.App (..)
  , I.Slice (..)
  , I.Scheduler (..)
  , I.PowerPolicy (..)
  , I.Power (..)
  , I.Monitoring (..)
  , I.ImageType (..)
  , I.Image (..)
  )
where

import Dhall
import qualified Nrm.Types.Manifest.Internal as I
import Protolude

-- As soon as Internal.Cfg isn't Interpretable, we write a dhall
-- interpretable datatype layer here. As it stands, this is a transitive
-- "identity" placeholder.

inputDManifest :: (MonadIO m) => Text -> m I.Manifest
inputDManifest fn =
  liftIO $ try (input dt fn) >>= \case
    Right d -> return d
    Left e -> throwError e
  where
    dt :: Dhall.Type I.Manifest
    dt = Dhall.auto

-- | converts from internal manifest datatype
toInternal :: I.Manifest -> I.Manifest
toInternal I.Manifest {..} = undefined

-- | converts to internal manifest datatype
fromInternal :: I.Manifest -> I.Manifest
fromInternal I.Manifest {..} = undefined

-- | reads a Dhall manifest
inputManifest :: (MonadIO m) => Text -> m I.Manifest
inputManifest fn = toInternal <$> inputDManifest fn
