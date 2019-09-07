{-# LANGUAGE DerivingVia #-}

{-# OPTIONS_GHC -fno-warn-orphans #-}

{-|
Module      : NRM.Types.Manifest
Copyright   : (c) UChicago Argonne, 2019
License     : BSD3
Maintainer  : fre@freux.fr
-}
module NRM.Types.Manifest
  ( Manifest (..)
  , App (..)
  , Slice (..)
  , Scheduler (..)
  , PowerPolicy (..)
  , Power (..)
  , Monitoring (..)
  , ImageType (..)
  , Perfwrapper (..)
  , Image (..)
  , jsonOptions
  )
where

import Data.Aeson
import Data.Default
import Data.JSON.Schema
import Data.MessagePack
import Data.Yaml.Internal ()
import Dhall hiding (Type)
import Generics.Generic.Aeson ()
import NRM.Classes.Messaging
import qualified NRM.Types.Units as U
import Protolude

data Manifest
  = Manifest
      { name :: Text
      , app :: App
      , hwbind :: Bool
      , image :: Image
      }
  deriving (Eq, Show, Generic, MessagePack, Interpret, Inject)
  deriving (JSONSchema, ToJSON, FromJSON) via GenericJSON Manifest

data SliceRuntime = Singularity | Nodeos | Dummy
  deriving (Eq, Show, Generic, MessagePack, Interpret)
  deriving (JSONSchema, ToJSON, FromJSON) via GenericJSON SliceRuntime

data App
  = App
      { slice :: Slice
      , scheduler :: Scheduler
      , perfwrapper :: Perfwrapper
      , power :: Power
      , monitoring :: Monitoring
      }
  deriving (Eq, Show, Generic, MessagePack, Interpret, Inject)
  deriving (JSONSchema, ToJSON, FromJSON) via GenericJSON App

data Slice
  = Slice
      { cpus :: Integer
      , mems :: Integer
      }
  deriving (Eq, Show, Generic, MessagePack, Interpret, Inject)
  deriving (JSONSchema, ToJSON, FromJSON) via GenericJSON Slice

data Scheduler = FIFO | HPC | Other Integer
  deriving (Eq, Show, Generic, MessagePack, Interpret, Inject)
  deriving (JSONSchema, ToJSON, FromJSON) via GenericJSON Scheduler

data PowerPolicy = NoPowerPolicy | DDCM | DVFS | Combined
  deriving (Eq, Show, Generic, MessagePack, Interpret, Inject)
  deriving (JSONSchema, ToJSON, FromJSON) via GenericJSON PowerPolicy

data Power
  = Power
      { policy :: PowerPolicy
      , profile :: Bool
      , slowdown :: Integer
      }
  deriving (Eq, Show, Generic, MessagePack, Interpret, Inject)
  deriving (JSONSchema, ToJSON, FromJSON) via GenericJSON Power

data Perfwrapper
  = PerfwrapperDisabled
  | Perfwrapper
      { perfFreq :: U.Frequency
      , perfLimit :: U.Operations
      }
  deriving (Eq, Show, Generic, MessagePack, Interpret, Inject)
  deriving (JSONSchema, ToJSON, FromJSON) via GenericJSON Perfwrapper

newtype Monitoring
  = Monitoring
      { ratelimit :: U.Frequency
      }
  deriving (Eq, Show, Generic, MessagePack, Interpret, Inject)
  deriving (JSONSchema, ToJSON, FromJSON) via GenericJSON Monitoring

data ImageType = Sif | Docker
  deriving (Eq, Show, Generic, MessagePack, Interpret, Inject)
  deriving (JSONSchema, ToJSON, FromJSON) via GenericJSON ImageType

data Image
  = Image
      { path :: Text
      , magetype :: ImageType
      , binds :: Maybe [Text]
      }
  | NoImage
  deriving (Eq, Show, Generic, MessagePack, Interpret, Inject)
  deriving (JSONSchema, ToJSON, FromJSON) via GenericJSON Image

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
    , perfwrapper = def
    , power = def
    , monitoring = def
    }

instance Default Perfwrapper where

  def = PerfwrapperDisabled

instance Default Monitoring where

  def = Monitoring {ratelimit = U.hz 1}

instance Default Image where

  def = NoImage

instance Default Slice where

  def = Slice {cpus = 1, mems = 1}

jsonOptions :: Options
jsonOptions = defaultOptions {omitNothingFields = True}

instance MessagePack Integer where

  toObject = toObject . (fromInteger :: Integer -> Int)

  fromObject x = (toInteger :: Int -> Integer) <$> fromObject x
