{-# LANGUAGE DerivingVia #-}

-- |
-- Module      : NRM.Types.Messaging.UpstreamPub
-- Copyright   : (c) UChicago Argonne, 2019
-- License     : BSD3
-- Maintainer  : fre@freux.fr
module NRM.Types.Messaging.UpstreamPub
  ( Pub (..),
    Progress (..),
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
  = -- | publishing some CPD measurements
    PubMeasurements Time [CPD.Measurement]
  | -- | publishing a new problem state
    PubCPD Time CPD.Problem
  | -- | publishing when a cpu counter message is received
    PubPerformance Time CmdID Operations
  | -- | publishing when a preloaded message is received
    PubPhaseContext Time DownstreamThreadID SliceID PhaseContext
  | -- | publishing when instrumentation produces progress reports
    PubProgress Time DownstreamThreadID Progress
  deriving (Show, Generic, MessagePack)
  deriving (JSONSchema, ToJSON, FromJSON) via GenericJSON Pub

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
