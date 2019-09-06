{-# LANGUAGE DerivingVia #-}

{-|
Module      : NRM.Types.Messaging.UpstreamReq
Copyright   : (c) UChicago Argonne, 2019
License     : BSD3
Maintainer  : fre@freux.fr
-}
module NRM.Types.Messaging.UpstreamReq
  ( Req (..)
  , Run (..)
  , SliceList (..)
  , CPD (..)
  , KillSlice (..)
  , KillCmd (..)
  , SetPower (..)
  , GetConfig (..)
  , GetState (..)
  )
where

import Data.Aeson
import Data.JSON.Schema
import Data.MessagePack
import NRM.Classes.Messaging
import NRM.Types.Manifest
import qualified NRM.Types.Cmd as Cmd
import qualified NRM.Types.Slice as C
import qualified NRM.Types.Units as U
import Protolude

data Req
  = ReqSliceList SliceList
  | ReqRun Run
  | ReqKillSlice KillSlice
  | ReqCPD CPD
  | ReqKillCmd KillCmd
  | ReqSetPower SetPower
  | ReqGetState GetState
  | ReqGetConfig GetConfig
  deriving (Show, Generic, MessagePack)
  deriving (JSONSchema, FromJSON, ToJSON) via GenericJSON Req

data Run
  = Run
      { manifest :: Manifest
      , spec :: Cmd.CmdSpec
      , runSliceID :: C.SliceID
      , detachCmd :: Bool
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
      { killCmdID :: Cmd.CmdID
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

instance NRMMessage Req Req where

  toJ = identity

  fromJ = identity

  messageSchema = gSchema
