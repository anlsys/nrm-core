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
import Dhall
import qualified Nrm.Types.Manifest.Dhall as D
import qualified Nrm.Types.Manifest.Internal as I
import Protolude
import System.IO.Error

data ContainerRuntime = Singularity | Nodeos | Dummy
  deriving (Generic, Interpret)

data App
  = App
      { slice :: Slice
      , scheduler :: Maybe Scheduler
      , perfwrapper :: Maybe Bool
      , power :: Maybe Power
      , monitoring :: Maybe Monitoring
      }
  deriving (Generic, Interpret, ToJSON)

instance FromJSON App where

  parseJSON = genericParseJSON defaultOptions {omitNothingFields = True}

data Slice
  = Slice
      { cpus :: Integer
      , mems :: Integer
      }
  deriving (Generic, Interpret, ToJSON)

instance FromJSON Slice where

  parseJSON = genericParseJSON defaultOptions {omitNothingFields = True}

data Scheduler = FIFO | HPC | Other Integer
  deriving (Generic, Interpret, ToJSON)

instance FromJSON Scheduler where

  parseJSON = genericParseJSON defaultOptions {omitNothingFields = True}

data PowerPolicy = NoPowerPolicy | DDCM | DVFS | Combined
  deriving (Generic, Interpret, ToJSON)

instance FromJSON PowerPolicy where

  parseJSON = genericParseJSON defaultOptions {omitNothingFields = True}

data Power
  = Power
      { policy :: PowerPolicy
      , profile :: Bool
      , slowdown :: Integer -- TODO shoul be <1
      }
  deriving (Generic, Interpret, ToJSON)

instance FromJSON Power where

  parseJSON = genericParseJSON defaultOptions {omitNothingFields = True}

newtype Monitoring
  = Monitoring
      { ratelimit :: Integer -- TODO >0
      }
  deriving (Generic, Interpret, ToJSON)

instance FromJSON Monitoring where

  parseJSON = genericParseJSON defaultOptions {omitNothingFields = True}

data Manifest
  = Manifest
      { name :: Text
      , version :: Text
      , app :: App
      , hwbind :: Maybe Bool
      , image :: Maybe Image
      }
  deriving (Generic, Interpret, ToJSON)

instance FromJSON Manifest where

  parseJSON = genericParseJSON defaultOptions {omitNothingFields = True}

data ImageType = Sif | Docker
  deriving (Generic, Interpret, ToJSON)

instance FromJSON ImageType where

  parseJSON = genericParseJSON defaultOptions {omitNothingFields = True}

data Image
  = Image
      { path :: Text
      , magetype :: ImageType
      , binds :: Maybe [Text]
      }
  deriving (Generic, Interpret, ToJSON)

instance FromJSON Image where

  parseJSON = genericParseJSON defaultOptions {omitNothingFields = True}

toInternal :: Manifest -> D.Manifest
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
