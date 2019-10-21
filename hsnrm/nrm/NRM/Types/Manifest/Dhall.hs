{-|
Module      : NRM.Types.Manifest.Dhall
Copyright   : (c) UChicago Argonne, 2019
License     : BSD3
Maintainer  : fre@freux.fr
-}
module NRM.Types.Manifest.Dhall
  ( inputManifest
  , toInternal
  , fromInternal
  , I.Manifest (..)
  , I.App (..)
  , I.Slice (..)
  , I.Scheduler (..)
  , I.PowerPolicy (..)
  , I.Power (..)
  , I.Instrumentation (..)
  , I.ImageType (..)
  , I.Image (..)
  )
where

import Dhall
import qualified NRM.Types.Manifest as I
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
toInternal = identity

-- | converts to internal manifest datatype
fromInternal :: I.Manifest -> I.Manifest
fromInternal = identity

-- | reads a Dhall manifest
inputManifest :: (MonadIO m) => Text -> m I.Manifest
inputManifest fn = toInternal <$> inputDManifest fn
