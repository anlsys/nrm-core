{-|
Module      : Nrm.Types.Manifest
Copyright   : (c) UChicago Argonne, 2019
License     : BSD3
Maintainer  : fre@freux.fr
-}
module Nrm.Types.Manifest
  ( Manifest (..)
  , App (..)
  , Slice (..)
  , Scheduler (..)
  , PowerPolicy (..)
  , Power (..)
  , Monitoring (..)
  , ImageType (..)
  , Image (..)
  )
where

import Data.Aeson
{-import Generics.Generic.Aeson-}
import Data.Default
import Data.JSON.Schema
import Data.Yaml ()
import Data.Yaml.Internal ()
import Dhall
import Protolude

data Manifest
  = Manifest
      { name :: Text
      , app :: App
      , hwbind :: Bool
      , image :: Image
      }
  deriving (Eq, Show, Generic, Interpret, Inject)

data ContainerRuntime = Singularity | Nodeos | Dummy
  deriving (Eq, Show, Generic, Interpret)

data App
  = App
      { slice :: Slice
      , scheduler :: Scheduler
      , perfwrapper :: Bool
      , power :: Power
      , monitoring :: Monitoring
      }
  deriving (Eq, Show, Generic, Interpret, Inject)

data Slice
  = Slice
      { cpus :: Integer
      , mems :: Integer
      }
  deriving (Eq, Show, Generic, Interpret, Inject)

data Scheduler = FIFO | HPC | Other Integer
  deriving (Eq, Show, Generic, Interpret, Inject)

data PowerPolicy = NoPowerPolicy | DDCM | DVFS | Combined
  deriving (Eq, Show, Generic, Interpret, Inject)

data Power
  = Power
      { policy :: PowerPolicy
      , profile :: Bool
      , slowdown :: Integer -- TODO shoul be <1
      }
  deriving (Eq, Show, Generic, Interpret, Inject)

newtype Monitoring
  = Monitoring
      { ratelimit :: Integer -- TODO >0
      }
  deriving (Eq, Show, Generic, Interpret, Inject)

data ImageType = Sif | Docker
  deriving (Eq, Show, Generic, Interpret, Inject)

data Image
  = Image
      { path :: Text
      , magetype :: ImageType
      , binds :: Maybe [Text]
      }
  | NoImage
  deriving (Eq, Show, Generic, Interpret, Inject)

instance Default Manifest where

  def = Manifest
    { name = "default"
    , app = def
    , hwbind = False
    , image = def
    }

instance Default Power where

  def = Power
    { policy = NoPowerPolicy
    , profile = False
    , slowdown = 1
    }

instance Default App where

  def = App
    { slice = def
    , scheduler = FIFO
    , perfwrapper = False
    , power = def
    , monitoring = def
    }

instance Default Monitoring where

  def = Monitoring {ratelimit = 1}

instance Default Image where

  def = NoImage

instance Default Slice where

  def = Slice {cpus = 1, mems = 1}

instance JSONSchema Manifest where

  schema = gSchema

instance JSONSchema App where

  schema = gSchema

instance JSONSchema Slice where

  schema = gSchema

instance JSONSchema Image where

  schema = gSchema

instance JSONSchema ImageType where

  schema = gSchema

instance JSONSchema Scheduler where

  schema = gSchema

instance JSONSchema Power where

  schema = gSchema

instance JSONSchema Monitoring where

  schema = gSchema

instance JSONSchema PowerPolicy where

  schema = gSchema

instance ToJSON Manifest where

  toEncoding = genericToEncoding jsonOptions

instance FromJSON Manifest where

  parseJSON = genericParseJSON jsonOptions

instance ToJSON App where

  toEncoding = genericToEncoding jsonOptions

instance FromJSON App where

  parseJSON = genericParseJSON jsonOptions

instance ToJSON Slice where

  toEncoding = genericToEncoding jsonOptions

instance FromJSON Slice where

  parseJSON = genericParseJSON jsonOptions

instance ToJSON Image where

  toEncoding = genericToEncoding jsonOptions

instance FromJSON Image where

  parseJSON = genericParseJSON jsonOptions

instance ToJSON ImageType where

  toEncoding = genericToEncoding jsonOptions

instance FromJSON ImageType where

  parseJSON = genericParseJSON jsonOptions

instance ToJSON Scheduler where

  toEncoding = genericToEncoding jsonOptions

instance FromJSON Scheduler where

  parseJSON = genericParseJSON jsonOptions

instance ToJSON Power where

  toEncoding = genericToEncoding jsonOptions

instance FromJSON Power where

  parseJSON = genericParseJSON jsonOptions

instance ToJSON Monitoring where

  toEncoding = genericToEncoding jsonOptions

instance FromJSON Monitoring where

  parseJSON = genericParseJSON jsonOptions

instance ToJSON PowerPolicy where

  toEncoding = genericToEncoding jsonOptions

instance FromJSON PowerPolicy where

  parseJSON = genericParseJSON jsonOptions


jsonOptions :: Options
jsonOptions = defaultOptions {omitNothingFields = True}
