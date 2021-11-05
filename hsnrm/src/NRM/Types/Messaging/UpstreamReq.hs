{-# LANGUAGE DerivingVia #-}

-- |
-- Module      : NRM.Types.Messaging.UpstreamReq
-- Copyright   : (c) UChicago Argonne, 2019
-- License     : BSD3
-- Maintainer  : swann@anl.gov
--
-- This module implements NRM's upstream API message formats.
module NRM.Types.Messaging.UpstreamReq
  ( Req (..),
    Run (..),
    SliceList (..),
    CPD (..),
    KillSlice (..),
    KillCmd (..),
    GetConfig (..),
    GetState (..),
  )
where

import CPD.Values
import Data.Aeson
import Data.JSON.Schema
import Data.MessagePack
import NRM.Classes.Messaging
import qualified NRM.Types.Cmd as Cmd
import NRM.Types.CmdID
import NRM.Types.Manifest
import qualified NRM.Types.Slice as C
import Protolude

data Req
  = -- | Request for the list of current slices.
    ReqSliceList SliceList
  | -- | Request to run a command.
    ReqRun Run
  | -- | Request to kill a whole slice.
    ReqKillSlice KillSlice
  | -- | Request to obtain the current control problem description.
    ReqCPD CPD
  | -- | Request to kill a running command.
    ReqKillCmd KillCmd
  | -- | Request to take some actions
    ReqActuate [Action]
  | -- | Request to obtain the full daemon state.
    ReqGetState GetState
  | -- | Request to get the daemon configuration.
    ReqGetConfig GetConfig
  deriving (Show, Generic, MessagePack, NRMMessage)

data Run
  = Run
      { manifest :: Manifest,
        spec :: Cmd.CmdSpec,
        runSliceID :: C.SliceID,
        detachCmd :: Bool
      }
  deriving (Show, Generic, MessagePack)
  deriving (JSONSchema, FromJSON, ToJSON) via GenericJSON Run

newtype KillSlice
  = KillSlice
      { killSliceID :: C.SliceID
      }
  deriving (Show, Generic, MessagePack)
  deriving (JSONSchema, FromJSON, ToJSON) via GenericJSON KillSlice

newtype KillCmd
  = KillCmd
      { killCmdID :: CmdID
      }
  deriving (Show, Generic, MessagePack)
  deriving (JSONSchema, FromJSON, ToJSON) via GenericJSON KillCmd

data SliceList = SliceList
  deriving (Show, Generic, MessagePack)
  deriving (JSONSchema, FromJSON, ToJSON) via GenericJSON SliceList

data CPD = CPD
  deriving (Show, Generic, MessagePack)
  deriving (JSONSchema, FromJSON, ToJSON) via GenericJSON CPD

data GetState = GetState
  deriving (Show, Generic, MessagePack)
  deriving (JSONSchema, FromJSON, ToJSON) via GenericJSON GetState

data GetConfig = GetConfig
  deriving (Show, Generic, MessagePack)
  deriving (JSONSchema, FromJSON, ToJSON) via GenericJSON GetConfig
