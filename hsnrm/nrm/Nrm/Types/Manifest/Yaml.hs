{-|
Module      : Nrm.Types.Manifest.Yaml
Copyright   : (c) UChicago Argonne, 2019
License     : BSD3
Maintainer  : fre@freux.fr
-}
module Nrm.Types.Manifest.Yaml
  (
  decodeManifestFile
  , decodeManifest
  , encodeManifest
  )
where

import Data.Aeson
import Data.Default
import qualified Data.Yaml
import qualified Nrm.Types.Manifest as I
import qualified Nrm.Types.Manifest.Dhall as D
import Protolude
import System.IO.Error

data ContainerRuntime = Singularity | Nodeos | Dummy
  deriving (Generic)

data Manifest
  = Manifest
      { name :: Text
      , app :: App
      , hwbind :: Maybe Bool
      , image :: Maybe I.Image
      }
  deriving (Generic)

data App
  = App
      { slice :: I.Slice
      , scheduler :: Maybe I.Scheduler
      , perfwrapper :: Maybe Bool
      , power :: Maybe I.Power
      , monitoring :: Maybe I.Monitoring
      }
  deriving (Generic)

instance FromJSON Manifest where

  parseJSON = genericParseJSON I.jsonOptions

instance FromJSON App where

  parseJSON = genericParseJSON I.jsonOptions

instance ToJSON Manifest where

  toJSON = genericToJSON I.jsonOptions
  toEncoding = genericToEncoding I.jsonOptions

instance ToJSON App where

  toJSON = genericToJSON I.jsonOptions
  toEncoding = genericToEncoding I.jsonOptions

toInternal :: Manifest -> D.Manifest
toInternal d = D.Manifest
  { name = name d
  , app = toInternalApp $ app d
  , hwbind = fromDefault hwbind D.hwbind
  , image = fromDefault image D.image
  }
  where
    fromDefault :: Default a => (Manifest -> Maybe c) -> (a -> c) -> c
    fromDefault attr attd = fromMaybe (attd def) (attr d)

toInternalApp :: App -> D.App
toInternalApp app = D.App
  { slice = slice app
  , scheduler = fromDefault scheduler D.scheduler
  , perfwrapper = fromDefault perfwrapper D.perfwrapper
  , power = fromDefault power D.power
  , monitoring = fromDefault monitoring D.monitoring
  }
  where
    fromDefault :: Default a => (App -> Maybe c) -> (a -> c) -> c
    fromDefault attr attd = fromMaybe (attd def) (attr app)

fromInternal :: D.Manifest -> Manifest
fromInternal m = Manifest
  { name = D.name m
  , app = fromInternalApp $ D.app m
  , hwbind = toJust D.hwbind
  , image = toJust D.image
  }
  where
    toJust :: (Eq a) => (D.Manifest -> a) -> Maybe a
    toJust x = if x (def :: D.Manifest) == xd then Nothing else Just xd
      where
        xd = x m

fromInternalApp :: D.App -> App
fromInternalApp a = App
  { slice = D.slice a
  , scheduler = toJust D.scheduler
  , perfwrapper = toJust D.perfwrapper
  , power = toJust D.power
  , monitoring = toJust D.monitoring
  }
  where
    toJust :: (Eq a) => (D.App -> a) -> Maybe a
    toJust x = if x (def :: D.App) == xd then Nothing else Just xd
      where
        xd = x a

decodeManifestFile :: (MonadIO m) => Text -> m I.Manifest
decodeManifestFile fn =
  liftIO $ try (Data.Yaml.decodeFileEither (toS fn)) >>= \case
    Left e -> throwError e
    Right (Left pa) -> throwError $ userError $ "parse fail:" <> show pa
    Right (Right a) -> return $ D.toInternal $ toInternal a

decodeManifest :: ByteString -> Either Data.Yaml.ParseException I.Manifest
decodeManifest fn = D.toInternal . toInternal <$> Data.Yaml.decodeEither' fn

encodeManifest :: I.Manifest -> ByteString
encodeManifest = Data.Yaml.encode . fromInternal . D.fromInternal
