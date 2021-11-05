{-# LANGUAGE DerivingVia #-}
{-# LANGUAGE TemplateHaskell #-}
{-# OPTIONS_GHC -fno-warn-orphans #-}
{-# OPTIONS_GHC -fno-warn-partial-fields #-}

-- |
-- Module      : NRM.Types.Configuration
-- Copyright   : (c) UChicago Argonne, 2019
-- License     : BSD3
-- Maintainer  : swann@anl.gov
module NRM.Types.Configuration
  ( Cfg (..),
    UpstreamCfg (..),
    DownstreamCfg (..),
    DaemonVerbosity (..),
    ControlCfg (..),
    RaplCfg (..),
    ExtraPassiveSensor (..),
    ActuatorKV (..),
    SensorKV (..),
    Power (..),
    Tag (..),
    Hint (..),
    SensorBehavior (..),
    ExtraActuator (..),
    LearnCfg (..),
    toInterval,
    toRange,
    toAction,
    toTime,
    toFrequency,
    toPower,
    jsonOptions,
  )
where

import CPD.Core
import CPD.Values hiding (actuatorValue)
import Data.Aeson
import Data.Default
import Data.Either.Validation as V
import Data.JSON.Schema ()
import Data.MessagePack
import Data.Yaml.Internal ()
import Dhall
import qualified Dhall.Core as Dhall
import Dhall.TH (HaskellType (..), makeHaskellTypes, staticDhallExpression)
import NRM.Classes.Messaging
import NRM.Orphans.Dhall ()
import qualified NRM.Types.Units as U
import Numeric.Interval
import Protolude

instance MessagePack Integer where

  toObject = toObject . (fromInteger :: Integer -> Int)

  fromObject x = (toInteger :: Int -> Integer) <$> fromObject x

makeHaskellTypes $
  let rPath :: Text
      rPath = "(./dhall/types/nrmd.dhall)."
      dRec :: Text -> Text -> HaskellType Text
      dRec s path = SingleConstructor s s (rPath <> path)
      dSum :: Text -> Text -> HaskellType Text
      dSum s path = MultipleConstructors s (rPath <> path)
   in [ dRec "Time" "Time",
        dRec "Power" "Power",
        dSum "DaemonVerbosity" "Verbosity",
        dSum "Tag" "Tag",
        dRec "Range" "Range",
        dRec "Frequency" "Frequency",
        dRec "ExtraActuator" "Actuator",
        dRec "ActuatorValue" "ActuatorValue",
        dSum "Hint" "Hint",
        dRec "Cfg" "Cfg",
        dSum "ControlCfg" "ControlCfg",
        dSum "LearnCfg" "LearnCfg",
        dRec "ActuatorKV" "ActuatorKV",
        dRec "SensorKV" "SensorKV",
        dRec "ExtraPassiveSensor" "Sensor",
        dRec "DownstreamCfg" "DownstreamCfg",
        dSum "SensorBehavior" "SensorBehavior",
        dRec "UpstreamCfg" "UpstreamCfg",
        dRec "RaplCfg" "RaplCfg"
      ]

instance Default Cfg where
  def =
    let expr :: Dhall.Expr s a
        expr =
          $( staticDhallExpression $
               "let t = ./dhall/types/nrmd.dhall"
                 <> " in ./dhall/defaults/nrmd.dhall : t.Cfg"
           )
     in (Dhall.extract (Dhall.auto :: Decoder Cfg) $! expr)
          & \case
            V.Failure _ -> panic "Error in default cfg"
            V.Success a -> a

toInterval :: Range -> Interval Double
toInterval r = upper r ... lower r

toRange :: Interval Double -> Range
toRange i = Range (inf i) (sup i)

toAction :: ActuatorValue -> Action
toAction a = Action (ActuatorID $ actuatorValueID a) (DiscreteDouble $ actuatorValue a)

toTime :: Time -> U.Time
toTime t@Time {} = U.uS $ microseconds t

toFrequency :: Frequency -> U.Frequency
toFrequency t@Frequency {} = U.hz $ hertz t

toPower :: Power -> U.Power
toPower t@Power {} = U.uW $ microwatts t

deriving instance Generic UpstreamCfg

deriving instance Eq UpstreamCfg

deriving instance Ord UpstreamCfg

deriving instance Show UpstreamCfg

deriving instance MessagePack UpstreamCfg

deriving instance FromDhall UpstreamCfg

deriving instance ToDhall UpstreamCfg

deriving via (GenericJSON UpstreamCfg) instance FromJSON UpstreamCfg

deriving via (GenericJSON UpstreamCfg) instance ToJSON UpstreamCfg

deriving via (GenericJSON UpstreamCfg) instance JSONSchema UpstreamCfg

deriving instance Generic RaplCfg

deriving instance Eq RaplCfg

deriving instance Ord RaplCfg

deriving instance Show RaplCfg

deriving instance MessagePack RaplCfg

deriving instance FromDhall RaplCfg

deriving instance ToDhall RaplCfg

deriving via (GenericJSON RaplCfg) instance FromJSON RaplCfg

deriving via (GenericJSON RaplCfg) instance ToJSON RaplCfg

deriving via (GenericJSON RaplCfg) instance JSONSchema RaplCfg

deriving instance Generic Tag

deriving instance Eq Tag

deriving instance Ord Tag

deriving instance Show Tag

deriving instance MessagePack Tag

deriving instance FromDhall Tag

deriving instance ToDhall Tag

deriving via (GenericJSON Tag) instance FromJSON Tag

deriving via (GenericJSON Tag) instance ToJSON Tag

deriving via (GenericJSON Tag) instance JSONSchema Tag

deriving instance Generic SensorBehavior

deriving instance Eq SensorBehavior

deriving instance Ord SensorBehavior

deriving instance Show SensorBehavior

deriving instance MessagePack SensorBehavior

deriving instance FromDhall SensorBehavior

deriving instance ToDhall SensorBehavior

deriving via (GenericJSON SensorBehavior) instance FromJSON SensorBehavior

deriving via (GenericJSON SensorBehavior) instance ToJSON SensorBehavior

deriving via (GenericJSON SensorBehavior) instance JSONSchema SensorBehavior

deriving instance Generic Range

deriving instance Eq Range

deriving instance Ord Range

deriving instance Show Range

deriving instance MessagePack Range

deriving instance FromDhall Range

deriving instance ToDhall Range

deriving via (GenericJSON Range) instance FromJSON Range

deriving via (GenericJSON Range) instance ToJSON Range

deriving via (GenericJSON Range) instance JSONSchema Range

deriving instance Generic ExtraPassiveSensor

deriving instance Eq ExtraPassiveSensor

deriving instance Ord ExtraPassiveSensor

deriving instance Show ExtraPassiveSensor

deriving instance MessagePack ExtraPassiveSensor

deriving instance FromDhall ExtraPassiveSensor

deriving instance ToDhall ExtraPassiveSensor

deriving via (GenericJSON ExtraPassiveSensor) instance FromJSON ExtraPassiveSensor

deriving via (GenericJSON ExtraPassiveSensor) instance ToJSON ExtraPassiveSensor

deriving via (GenericJSON ExtraPassiveSensor) instance JSONSchema ExtraPassiveSensor

deriving instance Generic SensorKV

deriving instance Eq SensorKV

deriving instance Ord SensorKV

deriving instance Show SensorKV

deriving instance MessagePack SensorKV

deriving instance FromDhall SensorKV

deriving instance ToDhall SensorKV

deriving via (GenericJSON SensorKV) instance FromJSON SensorKV

deriving via (GenericJSON SensorKV) instance ToJSON SensorKV

deriving via (GenericJSON SensorKV) instance JSONSchema SensorKV

deriving instance Generic ExtraActuator

deriving instance Eq ExtraActuator

deriving instance Ord ExtraActuator

deriving instance Show ExtraActuator

deriving instance MessagePack ExtraActuator

deriving instance FromDhall ExtraActuator

deriving instance ToDhall ExtraActuator

deriving via (GenericJSON ExtraActuator) instance FromJSON ExtraActuator

deriving via (GenericJSON ExtraActuator) instance ToJSON ExtraActuator

deriving via (GenericJSON ExtraActuator) instance JSONSchema ExtraActuator

deriving instance Generic DownstreamCfg

deriving instance Eq DownstreamCfg

deriving instance Ord DownstreamCfg

deriving instance Show DownstreamCfg

deriving instance MessagePack DownstreamCfg

deriving instance FromDhall DownstreamCfg

deriving instance ToDhall DownstreamCfg

deriving via (GenericJSON DownstreamCfg) instance FromJSON DownstreamCfg

deriving via (GenericJSON DownstreamCfg) instance ToJSON DownstreamCfg

deriving via (GenericJSON DownstreamCfg) instance JSONSchema DownstreamCfg

deriving instance Generic ActuatorKV

deriving instance Eq ActuatorKV

deriving instance Ord ActuatorKV

deriving instance Show ActuatorKV

deriving instance MessagePack ActuatorKV

deriving instance FromDhall ActuatorKV

deriving instance ToDhall ActuatorKV

deriving via (GenericJSON ActuatorKV) instance FromJSON ActuatorKV

deriving via (GenericJSON ActuatorKV) instance ToJSON ActuatorKV

deriving via (GenericJSON ActuatorKV) instance JSONSchema ActuatorKV

deriving instance Generic Power

deriving instance Eq Power

deriving instance Ord Power

deriving instance Show Power

deriving instance MessagePack Power

deriving instance FromDhall Power

deriving instance ToDhall Power

deriving via (GenericJSON Power) instance FromJSON Power

deriving via (GenericJSON Power) instance ToJSON Power

deriving via (GenericJSON Power) instance JSONSchema Power

deriving instance Generic Time

deriving instance Eq Time

deriving instance Ord Time

deriving instance Show Time

deriving instance MessagePack Time

deriving instance FromDhall Time

deriving instance ToDhall Time

deriving via (GenericJSON Time) instance FromJSON Time

deriving via (GenericJSON Time) instance ToJSON Time

deriving via (GenericJSON Time) instance JSONSchema Time

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

deriving instance Generic Hint

deriving instance Eq Hint

deriving instance Ord Hint

deriving instance Show Hint

deriving instance MessagePack Hint

deriving instance FromDhall Hint

deriving instance ToDhall Hint

deriving via (GenericJSON Hint) instance FromJSON Hint

deriving via (GenericJSON Hint) instance ToJSON Hint

deriving via (GenericJSON Hint) instance JSONSchema Hint

deriving instance Generic LearnCfg

deriving instance Eq LearnCfg

deriving instance Ord LearnCfg

deriving instance Show LearnCfg

deriving instance MessagePack LearnCfg

deriving instance FromDhall LearnCfg

deriving instance ToDhall LearnCfg

deriving via (GenericJSON LearnCfg) instance FromJSON LearnCfg

deriving via (GenericJSON LearnCfg) instance ToJSON LearnCfg

deriving via (GenericJSON LearnCfg) instance JSONSchema LearnCfg

deriving instance Generic ActuatorValue

deriving instance Eq ActuatorValue

deriving instance Ord ActuatorValue

deriving instance Show ActuatorValue

deriving instance MessagePack ActuatorValue

deriving instance FromDhall ActuatorValue

deriving instance ToDhall ActuatorValue

deriving via (GenericJSON ActuatorValue) instance FromJSON ActuatorValue

deriving via (GenericJSON ActuatorValue) instance ToJSON ActuatorValue

deriving via (GenericJSON ActuatorValue) instance JSONSchema ActuatorValue

deriving instance Generic ControlCfg

deriving instance Eq ControlCfg

deriving instance Ord ControlCfg

deriving instance Show ControlCfg

deriving instance MessagePack ControlCfg

deriving instance FromDhall ControlCfg

deriving instance ToDhall ControlCfg

deriving via (GenericJSON ControlCfg) instance FromJSON ControlCfg

deriving via (GenericJSON ControlCfg) instance ToJSON ControlCfg

deriving via (GenericJSON ControlCfg) instance JSONSchema ControlCfg

deriving instance Generic DaemonVerbosity

deriving instance Eq DaemonVerbosity

deriving instance Ord DaemonVerbosity

deriving instance Show DaemonVerbosity

deriving instance MessagePack DaemonVerbosity

deriving instance FromDhall DaemonVerbosity

deriving instance ToDhall DaemonVerbosity

deriving via (GenericJSON DaemonVerbosity) instance FromJSON DaemonVerbosity

deriving via (GenericJSON DaemonVerbosity) instance ToJSON DaemonVerbosity

deriving via (GenericJSON DaemonVerbosity) instance JSONSchema DaemonVerbosity

deriving instance Generic Cfg

deriving instance Eq Cfg

deriving instance Ord Cfg

deriving instance Show Cfg

deriving instance MessagePack Cfg

deriving instance FromDhall Cfg

deriving instance ToDhall Cfg

deriving via (GenericJSON Cfg) instance FromJSON Cfg

deriving via (GenericJSON Cfg) instance ToJSON Cfg

deriving via (GenericJSON Cfg) instance JSONSchema Cfg

jsonOptions :: Options
jsonOptions = defaultOptions
