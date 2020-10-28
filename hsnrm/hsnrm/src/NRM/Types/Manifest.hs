{-# LANGUAGE DerivingVia #-}
{-# LANGUAGE TemplateHaskell #-}
{-# OPTIONS_GHC -fno-warn-orphans #-}
{-# OPTIONS_GHC -fno-warn-partial-fields #-}

-- |
-- Module      : NRM.Types.Manifest
-- Copyright   : (c) UChicago Argonne, 2019
-- License     : BSD3
-- Maintainer  : fre@freux.fr
module NRM.Types.Manifest
  ( Manifest (..),
    App (..),
    Slice (..),
    Scheduler (..),
    PowerPolicy (..),
    PowerCfg (..),
    Instrumentation (..),
    ImageType (..),
    Perfwrapper (..),
    Image (..),
    toFrequency,
    jsonOptions,
  )
where

import Data.Aeson
import Data.Default
import Data.Either.Validation as V
import Data.JSON.Schema
import Data.MessagePack
import Data.Yaml.Internal ()
import Dhall
import qualified Dhall.Core as Dhall
import Dhall.TH (HaskellType (..), makeHaskellTypes, staticDhallExpression)
import Generics.Generic.Aeson ()
import NRM.Classes.Messaging
import qualified NRM.Types.Units as U
import Protolude

makeHaskellTypes $
  let rPath :: Text
      rPath = "(./dhall/types/manifest.dhall)."
      dRec :: Text -> Text -> HaskellType Text
      dRec s path = SingleConstructor s s (rPath <> path)
      dSum :: Text -> Text -> HaskellType Text
      dSum s path = MultipleConstructors s (rPath <> path)
   in [ dSum "Scheduler" "Scheduler",
        dRec "Frequency" "Frequency",
        dRec "Perfwrapper" "Perfwrapper",
        dSum "ImageType" "ImageType",
        dRec "Image" "Image",
        dSum "PowerPolicy" "PowerPolicy",
        dRec "Slice" "Slice",
        dRec "PowerCfg" "PowerCfg",
        dRec "Instrumentation" "Instrumentation",
        dRec "App" "App",
        dRec "Manifest" "Manifest"
      ]

toFrequency :: Frequency -> U.Frequency
toFrequency t@Frequency {} = U.hz $ hertz t

instance Default Manifest where
  def =
    let expr :: Dhall.Expr s a
        expr =
          $( staticDhallExpression $
               "let t = ./dhall/types/manifest.dhall"
                 <> " in ./dhall/defaults/manifest.dhall : t.Manifest"
           )
     in (Dhall.extract (Dhall.auto :: Decoder Manifest) $! expr)
          & \case
            V.Failure _ -> panic "Error in default cfg"
            V.Success a -> a

jsonOptions :: Options
jsonOptions = defaultOptions {omitNothingFields = False}

instance MessagePack Integer where

  toObject = toObject . (fromInteger :: Integer -> Int)

  fromObject x = (toInteger :: Int -> Integer) <$> fromObject x

deriving instance Generic Scheduler

deriving instance Eq Scheduler

deriving instance Ord Scheduler

deriving instance Show Scheduler

deriving instance MessagePack Scheduler

deriving instance FromDhall Scheduler

deriving instance ToDhall Scheduler

deriving via (GenericJSON Scheduler) instance FromJSON Scheduler

deriving via (GenericJSON Scheduler) instance ToJSON Scheduler

deriving via (GenericJSON Scheduler) instance JSONSchema Scheduler

deriving instance Generic Frequency

deriving instance Eq Frequency

deriving instance Ord Frequency

deriving instance Show Frequency

deriving instance MessagePack Frequency

deriving instance FromDhall Frequency

deriving instance ToDhall Frequency

deriving via (GenericJSON Frequency) instance FromJSON Frequency

deriving via (GenericJSON Frequency) instance ToJSON Frequency

deriving via (GenericJSON Frequency) instance JSONSchema Frequency

deriving instance Generic Perfwrapper

deriving instance Eq Perfwrapper

deriving instance Ord Perfwrapper

deriving instance Show Perfwrapper

deriving instance MessagePack Perfwrapper

deriving instance FromDhall Perfwrapper

deriving instance ToDhall Perfwrapper

deriving via (GenericJSON Perfwrapper) instance FromJSON Perfwrapper

deriving via (GenericJSON Perfwrapper) instance ToJSON Perfwrapper

deriving via (GenericJSON Perfwrapper) instance JSONSchema Perfwrapper

deriving instance Generic ImageType

deriving instance Eq ImageType

deriving instance Ord ImageType

deriving instance Show ImageType

deriving instance MessagePack ImageType

deriving instance FromDhall ImageType

deriving instance ToDhall ImageType

deriving via (GenericJSON ImageType) instance FromJSON ImageType

deriving via (GenericJSON ImageType) instance ToJSON ImageType

deriving via (GenericJSON ImageType) instance JSONSchema ImageType

deriving instance Generic Image

deriving instance Eq Image

deriving instance Ord Image

deriving instance Show Image

deriving instance MessagePack Image

deriving instance FromDhall Image

deriving instance ToDhall Image

deriving via (GenericJSON Image) instance FromJSON Image

deriving via (GenericJSON Image) instance ToJSON Image

deriving via (GenericJSON Image) instance JSONSchema Image

deriving instance Generic PowerPolicy

deriving instance Eq PowerPolicy

deriving instance Ord PowerPolicy

deriving instance Show PowerPolicy

deriving instance MessagePack PowerPolicy

deriving instance FromDhall PowerPolicy

deriving instance ToDhall PowerPolicy

deriving via (GenericJSON PowerPolicy) instance FromJSON PowerPolicy

deriving via (GenericJSON PowerPolicy) instance ToJSON PowerPolicy

deriving via (GenericJSON PowerPolicy) instance JSONSchema PowerPolicy

deriving instance Generic Slice

deriving instance Eq Slice

deriving instance Ord Slice

deriving instance Show Slice

deriving instance MessagePack Slice

deriving instance FromDhall Slice

deriving instance ToDhall Slice

deriving via (GenericJSON Slice) instance FromJSON Slice

deriving via (GenericJSON Slice) instance ToJSON Slice

deriving via (GenericJSON Slice) instance JSONSchema Slice

deriving instance Generic PowerCfg

deriving instance Eq PowerCfg

deriving instance Ord PowerCfg

deriving instance Show PowerCfg

deriving instance MessagePack PowerCfg

deriving instance FromDhall PowerCfg

deriving instance ToDhall PowerCfg

deriving via (GenericJSON PowerCfg) instance FromJSON PowerCfg

deriving via (GenericJSON PowerCfg) instance ToJSON PowerCfg

deriving via (GenericJSON PowerCfg) instance JSONSchema PowerCfg

deriving instance Generic Instrumentation

deriving instance Eq Instrumentation

deriving instance Ord Instrumentation

deriving instance Show Instrumentation

deriving instance MessagePack Instrumentation

deriving instance FromDhall Instrumentation

deriving instance ToDhall Instrumentation

deriving via (GenericJSON Instrumentation) instance FromJSON Instrumentation

deriving via (GenericJSON Instrumentation) instance ToJSON Instrumentation

deriving via (GenericJSON Instrumentation) instance JSONSchema Instrumentation

deriving instance Generic App

deriving instance Eq App

deriving instance Ord App

deriving instance Show App

deriving instance MessagePack App

deriving instance FromDhall App

deriving instance ToDhall App

deriving via (GenericJSON App) instance FromJSON App

deriving via (GenericJSON App) instance ToJSON App

deriving via (GenericJSON App) instance JSONSchema App

deriving instance Generic Manifest

deriving instance Eq Manifest

deriving instance Ord Manifest

deriving instance Show Manifest

deriving instance MessagePack Manifest

deriving instance FromDhall Manifest

deriving instance ToDhall Manifest

deriving via (GenericJSON Manifest) instance FromJSON Manifest

deriving via (GenericJSON Manifest) instance ToJSON Manifest

deriving via (GenericJSON Manifest) instance JSONSchema Manifest
