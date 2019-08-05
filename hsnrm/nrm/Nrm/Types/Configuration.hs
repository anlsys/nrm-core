{-|
Module      : Nrm.Types.Configuration
Description : Nrm configuration
Copyright   : (c) UChicago Argonne, 2019
License     : BSD3
Maintainer  : fre@freux.fr
-}
module Nrm.Types.Configuration
  ( Cfg (..)
  , ApplicationManifest (..)
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

data Cfg
  = Cfg
      { logfile :: Text
      , hwloc :: Text
      , perf :: Text
      , argo_perf_wrapper :: Text
      , argo_nodeos_config :: Text
      , pmpi_lib :: Text
      , singularity :: Text
      , container_runtime :: ContainerRuntime
      }
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

data ApplicationManifest
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
