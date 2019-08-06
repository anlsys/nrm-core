{-|
Module      : Nrm.Types.Manifest.Yaml
Description : Nrm application manifest yaml reader
Copyright   : (c) UChicago Argonne, 2019
License     : BSD3
Maintainer  : fre@freux.fr
-}
module Nrm.Types.Manifest.Yaml
  ( Manifest (..)
  , decodeManifestFile
  , decodeManifest
  , encodeManifest
  )
where

import Data.Aeson
import Data.Yaml
import qualified Nrm.Types.Manifest.Dhall as D
import qualified Nrm.Types.Manifest.Internal as I
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
  deriving (Generic, ToJSON)

data App
  = App
      { slice :: I.Slice
      , scheduler :: Maybe I.Scheduler
      , perfwrapper :: Maybe Bool
      , power :: Maybe I.Power
      , monitoring :: Maybe I.Monitoring
      }
  deriving (Generic, ToJSON)

instance FromJSON Manifest where

  parseJSON = genericParseJSON defaultOptions {omitNothingFields = True}

instance FromJSON App where

  parseJSON = genericParseJSON defaultOptions {omitNothingFields = True}

toInternal :: Manifest -> D.Manifest
toInternal d = undefined

{-toInternal d = D.Manifest-}
{-{ cmds = toInternalCmd <$> cmds d-}
{-, verbose = fromMaybe False (verbose d)-}
{-, cleaning = fromMaybe False (cleaning d)-}
{-, pre = fromMaybe [] (pre d)-}
{-, post = fromMaybe [] (post d)-}
{-, workdir = fromMaybe "./" (workdir d)-}
{-}-}
fromInternal :: D.Manifest -> Manifest
fromInternal d = undefined

{-Manifest {..}-}
{-where-}
{-workdir = case DT.workdir d of-}
{-"./" -> Nothing-}
{-w    -> Just w-}
{-cmds     = fromInternalCmd <$> DT.cmds d-}
{-verbose  = if DT.verbose d then Just True else Nothing-}
{-cleaning = if DT.cleaning d then Just True else Nothing-}
{-pre      = case DT.pre d of-}
{-[] -> Nothing-}
{-l  -> Just l-}
{-post = case DT.post d of-}
{-[] -> Nothing-}
{-l  -> Just l-}
decodeManifestFile :: (MonadIO m) => Text -> m I.Manifest
decodeManifestFile fn =
  liftIO $ try (decodeFileEither (toS fn)) >>= \case
    Left e -> throwError e
    Right (Left pa) -> throwError $ userError $ "parse fail:" <> show pa
    Right (Right a) -> return $ D.toInternal $ toInternal a

decodeManifest :: ByteString -> Either ParseException I.Manifest
decodeManifest fn = D.toInternal . toInternal <$> decodeEither' fn

encodeManifest :: I.Manifest -> ByteString
encodeManifest = Data.Yaml.encode . fromInternal . D.fromInternal
