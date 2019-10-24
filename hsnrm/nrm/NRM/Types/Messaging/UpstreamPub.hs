{-# LANGUAGE DerivingVia #-}

-- |
-- Module      : NRM.Types.Messaging.UpstreamPub
-- Copyright   : (c) UChicago Argonne, 2019
-- License     : BSD3
-- Maintainer  : fre@freux.fr
module NRM.Types.Messaging.UpstreamPub
  ( Pub (..),
    Progress (..),
    Power (..),
    SliceStart (..),
    SliceExit (..),
  )
where

import CPD.Core as CPD
import CPD.Values as CPD
import Data.Aeson
import Data.JSON.Schema
import Data.MessagePack
import NRM.Classes.Messaging
import NRM.Types.CmdID
import NRM.Types.DownstreamThreadID
import NRM.Types.Messaging.DownstreamEvent
import NRM.Types.Slice as C
import NRM.Types.Units
import Protolude

data Pub
  = PubMeasurements Time [CPD.Measurement]
  | PubCPD Time CPD.Problem
  | PubPower Time PowerState
  | PubSliceStart Time SliceStart
  | PubSliceExit Time SliceExit
  | PubPerformance Time CmdID Operations
  | PubPhaseContext Time DownstreamThreadID PhaseContext
  | PubProgress Time Progress
  deriving (Show, Generic, MessagePack)
  deriving (JSONSchema, ToJSON, FromJSON) via GenericJSON Pub

data PowerState
  = PowerState
      { total :: Power,
        limit :: Power
      }
  deriving (Show, Generic, MessagePack)
  deriving (JSONSchema, ToJSON, FromJSON) via GenericJSON PowerState

data SliceStart
  = SliceStart
      { startSliceID :: C.SliceID,
        errno :: Int,
        power :: Power
      }
  deriving (Show, Generic, MessagePack)
  deriving (JSONSchema, ToJSON, FromJSON) via GenericJSON SliceStart

data SliceExit
  = SliceExit
      { exitSliceID :: C.SliceID,
        profile_data :: Map Text Text
      }
  deriving (Show, Generic, MessagePack)
  deriving (JSONSchema, ToJSON, FromJSON) via GenericJSON SliceExit

data Control
  = Control
      { powercap :: Power,
        energy :: Energy,
        performance :: Operations,
        control_time :: Time,
        feedback_time :: Time
      }
  deriving (Show, Generic, MessagePack)
  deriving (JSONSchema, ToJSON, FromJSON) via GenericJSON Control

instance NRMMessage Pub Pub where

  fromJ = identity

  toJ = identity
