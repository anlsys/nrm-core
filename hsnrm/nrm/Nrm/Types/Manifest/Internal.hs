{-|
Module      : Nrm.Types.Manifest.Internal
Description : Nrm application manifest
Copyright   : (c) UChicago Argonne, 2019
License     : BSD3
Maintainer  : fre@freux.fr
-}
module Nrm.Types.Manifest.Internal
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
import Data.Default
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
  deriving (Generic, Interpret)

instance Default Manifest where

  def = Manifest
    { name = "default"
    }

data ContainerRuntime = Singularity | Nodeos | Dummy
  deriving (Generic, Interpret)

data App
  = App
      { slice :: Slice
      , scheduler :: Scheduler
      , perfwrapper :: Bool
      , power :: Power
      , monitoring :: Monitoring
      }
  deriving (Generic, Interpret)

data Slice
  = Slice
      { cpus :: Integer
      , mems :: Integer
      }
  deriving (Generic, Interpret, ToJSON)

data Scheduler = FIFO | HPC | Other Integer
  deriving (Generic, Interpret, ToJSON)

data PowerPolicy = NoPowerPolicy | DDCM | DVFS | Combined
  deriving (Generic, Interpret, ToJSON)

data Power
  = Power
      { policy :: PowerPolicy
      , profile :: Bool
      , slowdown :: Integer -- TODO shoul be <1
      }
  deriving (Generic, Interpret, ToJSON)

newtype Monitoring
  = Monitoring
      { ratelimit :: Integer -- TODO >0
      }
  deriving (Generic, Interpret, ToJSON)

data ImageType = Sif | Docker
  deriving (Generic, Interpret, ToJSON)

data Image
  = Image
      { path :: Text
      , magetype :: ImageType
      , binds :: Maybe [Text]
      }
  deriving (Generic, Interpret, ToJSON)

instance FromJSON App where

  parseJSON = genericParseJSON defaultOptions {omitNothingFields = True}

instance FromJSON Slice where

  parseJSON = genericParseJSON defaultOptions {omitNothingFields = True}

instance FromJSON Scheduler where

  parseJSON = genericParseJSON defaultOptions {omitNothingFields = True}

instance FromJSON PowerPolicy where

  parseJSON = genericParseJSON defaultOptions {omitNothingFields = True}

instance FromJSON Power where

  parseJSON = genericParseJSON defaultOptions {omitNothingFields = True}

instance FromJSON Monitoring where

  parseJSON = genericParseJSON defaultOptions {omitNothingFields = True}

instance FromJSON Manifest where

  parseJSON = genericParseJSON defaultOptions {omitNothingFields = True}

instance FromJSON ImageType where

  parseJSON = genericParseJSON defaultOptions {omitNothingFields = True}

instance FromJSON Image where

  parseJSON = genericParseJSON defaultOptions {omitNothingFields = True}

