{-# LANGUAGE DerivingVia #-}

-- |
-- Module      : NRM.Types.Messaging.UpstreamPub
-- Copyright   : (c) UChicago Argonne, 2019
-- License     : BSD3
-- Maintainer  : fre@freux.fr
--
-- This module implements NRM's upstream API's published message formats.
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
import NRM.Types.Controller
import NRM.Types.DownstreamThreadID
import NRM.Types.Messaging.DownstreamEvent
import NRM.Types.Slice as C
import NRM.Types.Units
import Protolude

data Pub
  = -- | Publishing some CPD measurements.
    PubMeasurements Time [CPD.Measurement]
  | -- | Publishing a new problem state.
    PubCPD Time CPD.Problem
  | -- | Publishing when a cpu counter message is received.
    PubPerformance Time CmdID Operations
  | -- | Publishing when a preloaded message is received.
    PubPhaseContext Time DownstreamThreadID SliceID PhaseContext
  | -- | Publishing when instrumentation produces progress reports.
    PubProgress Time DownstreamThreadID Progress
  | -- | Publishing when an action was taken.
    PubAction Time [CPD.Action] DecisionMetadata Controller
  | -- | Publishing when rewards are computed.
    PubReward Time CPD.Problem Double
  deriving (Show, Generic, MessagePack, NRMMessage)

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
