{-# LANGUAGE DerivingVia #-}

{-|
Module      : NRM.Types.Messaging.UpstreamPub
Copyright   : (c) UChicago Argonne, 2019
License     : BSD3
Maintainer  : fre@freux.fr
-}
module NRM.Types.Messaging.UpstreamPub
  ( Pub (..)
  )
where

import Data.Aeson
import Data.JSON.Schema
import Data.MessagePack
import NRM.Classes.Messaging
import NRM.Types.DownstreamThreadClient as D
import NRM.Types.Slice as C
import qualified NRM.Types.Units as U
import Protolude

data Pub
  = PubPower Power
  | PubSliceStart SliceStart
  | PubSliceExit SliceExit
  | PubPerformance Performance
  | PubProgress Progress
  | PubControl Control
  deriving (Show, Generic, MessagePack)
  deriving (JSONSchema, ToJSON, FromJSON) via GenericJSON Pub

data Power
  = Power
      { total :: U.Power
      , limit :: U.Power
      }
  deriving (Show, Generic, MessagePack)
  deriving (JSONSchema, ToJSON, FromJSON) via GenericJSON Power

data SliceStart
  = SliceStart
      { startSliceID :: C.SliceID
      , errno :: Int
      , power :: U.Power
      }
  deriving (Show, Generic, MessagePack)
  deriving (JSONSchema, ToJSON, FromJSON) via GenericJSON SliceStart

data SliceExit
  = SliceExit
      { exitSliceID :: C.SliceID
      , profile_data :: Map Text Text
      }
  deriving (Show, Generic, MessagePack)
  deriving (JSONSchema, ToJSON, FromJSON) via GenericJSON SliceExit

data Performance
  = Performance
      { perfSliceID :: C.SliceID
      , perf :: U.Operations
      }
  deriving (Show, Generic, MessagePack)
  deriving (JSONSchema, ToJSON, FromJSON) via GenericJSON Performance

data Progress
  = Progress
      { downstreamThreadID :: D.DownstreamThreadID
      , payload :: U.Progress
      }
  deriving (Show, Generic, MessagePack)
  deriving (JSONSchema, ToJSON, FromJSON) via GenericJSON Progress

data Control
  = Control
      { powercap :: U.Power
      , energy :: U.Energy
      , performance :: U.Operations
      , control_time :: U.Time
      , feedback_time :: U.Time
      }
  deriving (Show, Generic, MessagePack)
  deriving (JSONSchema, ToJSON, FromJSON) via GenericJSON Control

instance NRMMessage Pub Pub where

  fromJ = identity

  toJ = identity
