{-|
Module      : Nrm.Types.Manifest.Yaml
Description : Nrm application manifest
Copyright   : (c) UChicago Argonne, 2019
License     : BSD3
Maintainer  : fre@freux.fr
-}
module Nrm.Types.Manifest.Yaml
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

import Dhall
import Protolude

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
  deriving (Generic, Interpret)

data Slice
  = Slice
      { cpus :: Integer
      , mems :: Integer
      }
  deriving (Generic, Interpret)

data Scheduler = FIFO | HPC | Other Integer
  deriving (Generic, Interpret)

data PowerPolicy = NoPowerPolicy | DDCM | DVFS | Combined
  deriving (Generic, Interpret)

data Power
  = Power
      { policy :: PowerPolicy
      , profile :: Bool
      , slowdown :: Integer -- TODO shoul be <1
      }
  deriving (Generic, Interpret)

newtype Monitoring
  = Monitoring
      { ratelimit :: Integer -- TODO >0
      }
  deriving (Generic, Interpret)

data Manifest
  = Manifest
      { name :: Text
      , version :: Text
      , app :: App
      , hwbind :: Maybe Bool
      , image :: Maybe Image
      }
  deriving (Generic, Interpret)

data ImageType = Sif | Docker
  deriving (Generic, Interpret)

data Image
  = Image
      { path :: Text
      , magetype :: ImageType
      , binds :: Maybe [Text]
      }
  deriving (Generic, Interpret)
