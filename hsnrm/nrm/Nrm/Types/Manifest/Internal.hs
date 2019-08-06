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
  , ClientVerbosity (..)
  )
where

import Data.Default
import Data.Yaml.Internal
import Dhall
import qualified Nrm.Version as V (version)
import Protolude

data ClientVerbosity = Normal | Verbose
  deriving (Eq, Generic, Interpret)

data Manifest
  = Manifest
      { name :: Text
      , app :: App
      , hwbind :: Bool
      , image :: Image
      , verbose :: ClientVerbosity
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

data ImageType = Sif | Docker
  deriving (Generic, Interpret)

data Image
  = Image
      { path :: Text
      , magetype :: ImageType
      , binds :: Maybe [Text]
      }
  deriving (Generic, Interpret)
