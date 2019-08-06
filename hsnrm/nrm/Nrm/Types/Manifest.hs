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
  | NoImage
  deriving (Generic, Interpret, ToJSON)

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
