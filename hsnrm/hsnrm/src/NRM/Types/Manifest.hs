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
    AppActuator (..),
    AppActuatorKV (..),
    Instrumentation (..),
    Perfwrapper (..),
    toFrequency,
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
   in [ dRec "Frequency" "Frequency",
        dRec "Perfwrapper" "Perfwrapper",
        dRec "Instrumentation" "Instrumentation",
        dRec "AppActuator" "AppActuator",
        dRec "AppActuatorKV" "AppActuatorKV",
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

instance MessagePack Integer where

  toObject = toObject . (fromInteger :: Integer -> Int)

  fromObject x = (toInteger :: Int -> Integer) <$> fromObject x

deriving instance Generic AppActuatorKV

deriving instance Eq AppActuatorKV

deriving instance Ord AppActuatorKV

deriving instance Show AppActuatorKV

deriving instance MessagePack AppActuatorKV

deriving instance FromDhall AppActuatorKV

deriving instance ToDhall AppActuatorKV

deriving via (GenericJSON AppActuatorKV) instance FromJSON AppActuatorKV

deriving via (GenericJSON AppActuatorKV) instance ToJSON AppActuatorKV

deriving via (GenericJSON AppActuatorKV) instance JSONSchema AppActuatorKV

deriving instance Generic AppActuator

deriving instance Eq AppActuator

deriving instance Ord AppActuator

deriving instance Show AppActuator

deriving instance MessagePack AppActuator

deriving instance FromDhall AppActuator

deriving instance ToDhall AppActuator

deriving via (GenericJSON AppActuator) instance FromJSON AppActuator

deriving via (GenericJSON AppActuator) instance ToJSON AppActuator

deriving via (GenericJSON AppActuator) instance JSONSchema AppActuator

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
