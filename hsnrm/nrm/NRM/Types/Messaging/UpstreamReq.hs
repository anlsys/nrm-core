{-# LANGUAGE DerivingVia #-}

-- |
-- Module      : NRM.Types.Messaging.UpstreamReq
-- Copyright   : (c) UChicago Argonne, 2019
-- License     : BSD3
-- Maintainer  : fre@freux.fr
module NRM.Types.Messaging.UpstreamReq
  ( Req (..),
    Run (..),
    SliceList (..),
    CPD (..),
    KillSlice (..),
    KillCmd (..),
    SetPower (..),
    GetConfig (..),
    GetState (..),
  )
where

import Data.Aeson
import Data.JSON.Schema
import Data.MessagePack
import NRM.Classes.Messaging
import qualified NRM.Types.Cmd as Cmd
import NRM.Types.CmdID
import NRM.Types.Manifest
import qualified NRM.Types.Slice as C
import qualified NRM.Types.Units as U
import Protolude

data Req
  = -- | encodes a request for the list of current slices
    ReqSliceList SliceList
  | -- | encodes a request to run a command
    ReqRun Run
  | -- | encodes a request to kill a whole slice
    ReqKillSlice KillSlice
  | -- | encodes a request to obtain the current control problem description
    ReqCPD CPD
  | -- | encodes a request to kill a running command
    ReqKillCmd KillCmd
  | -- | encodes a request to set the global power bound
    ReqSetPower SetPower
  | -- | encodes a request to obtain the full daemon state
    ReqGetState GetState
  | -- | encodes a request to get the daemon configuration
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

newtype SetPower
  = SetPower
      { limit :: U.Power
      }
  deriving (Show, Generic, MessagePack)
  deriving (JSONSchema, FromJSON, ToJSON) via GenericJSON SetPower

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
