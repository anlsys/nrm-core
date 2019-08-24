{-# OPTIONS_GHC -fno-warn-orphans #-}
{-|
Module      : Nrm.Types.Messaging.UpstreamRep
Copyright   : (c) UChicago Argonne, 2019
License     : BSD3
Maintainer  : fre@freux.fr
-}
module Nrm.Types.Messaging.UpstreamRep
  ( Rep (..)
  , ContainerList (..)
  , Stdout (..)
  , Stderr (..)
  , Start (..)
  , ContainerDeath (..)
  , EndStream (..)
  , CmdDeath (..)
  , GetPower (..)
  , GetConfig (..)
  , GetState (..)
  )
where

import Data.Aeson
import Data.JSON.Schema
import Data.MessagePack
import Generics.Generic.Aeson
import Nrm.Classes.Messaging
import Nrm.Types.Configuration as C
import Nrm.Types.Container as C
import Nrm.Types.NrmState as S
import Nrm.Types.Process as P
import Nrm.Types.Units as U
import Protolude hiding (Rep)

data Rep
  = RepList ContainerList
  | RepStdout Stdout
  | RepStderr Stderr
  | RepStart Start
  | RepEndStream EndStream
  | RepContainerDeath ContainerDeath
  | RepCmdDeath CmdDeath
  | RepGetPower GetPower
  | RepGetState GetState
  | RepGetConfig GetConfig
  deriving (Show, Generic, MessagePack)

data EndStream
  = EndStream
  deriving (Show, Generic, MessagePack)

newtype ContainerList
  = ContainerList
      { containers :: [(C.ContainerID, C.Container)]
      }
  deriving (Show, Generic, MessagePack)

data Stdout
  = Stdout
      { stdoutContainerID :: C.ContainerID
      , stdoutPayload :: Text
      }
  deriving (Show, Generic, MessagePack)

data Stderr
  = Stderr
      { stderrContainerID :: C.ContainerID
      , stderrPayload :: Text
      }
  deriving (Show, Generic, MessagePack)

data Start
  = Start
      { startContainerID :: C.ContainerID
      , startCmdID :: P.CmdID
      }
  deriving (Show, Generic, MessagePack)

data CmdDeath
  = CmdDeath
      { deathCmdID :: Text
      , status :: Int
      }
  deriving (Show, Generic, MessagePack)

newtype ContainerDeath
  = ContainerDeath
      { deathContainerID :: Text
      }
  deriving (Show, Generic, MessagePack)

newtype GetPower
  = GetPower
      { limit :: U.Power
      }
  deriving (Show, Generic, MessagePack)

newtype GetState
  = GetState
      { state :: S.NrmState
      }
  deriving (Show, Generic, MessagePack)

newtype GetConfig
  = GetConfig
      { config :: C.Cfg
      }
  deriving (Show, Generic, MessagePack)

instance NrmMessage Rep Rep where

  fromJ = identity

  toJ = identity

instance ToJSON EndStream where

  toJSON = gtoJson

instance FromJSON EndStream where

  parseJSON = gparseJson

instance JSONSchema EndStream where

  schema = gSchema

instance ToJSON GetPower where

  toJSON = gtoJson

instance FromJSON GetPower where

  parseJSON = gparseJson

instance JSONSchema GetPower where

  schema = gSchema

instance ToJSON ContainerList where

  toJSON = gtoJson

instance FromJSON ContainerList where

  parseJSON = gparseJson

instance JSONSchema ContainerList where

  schema = gSchema

instance ToJSON CmdDeath where

  toJSON = gtoJson

instance FromJSON CmdDeath where

  parseJSON = gparseJson

instance JSONSchema CmdDeath where

  schema = gSchema

instance ToJSON ContainerDeath where

  toJSON = gtoJson

instance FromJSON ContainerDeath where

  parseJSON = gparseJson

instance JSONSchema ContainerDeath where

  schema = gSchema

instance ToJSON Stderr where

  toJSON = gtoJson

instance FromJSON Stderr where

  parseJSON = gparseJson

instance JSONSchema Stderr where

  schema = gSchema

instance ToJSON Stdout where

  toJSON = gtoJson

instance FromJSON Stdout where

  parseJSON = gparseJson

instance JSONSchema Stdout where

  schema = gSchema

instance ToJSON Start where

  toJSON = gtoJson

instance FromJSON Start where

  parseJSON = gparseJson

instance JSONSchema Start where

  schema = gSchema

instance ToJSON GetState where

  toJSON = gtoJson

instance FromJSON GetState where

  parseJSON = gparseJson

instance JSONSchema GetState where

  schema = gSchema

instance ToJSON GetConfig where

  toJSON = gtoJson

instance FromJSON GetConfig where

  parseJSON = gparseJson

instance JSONSchema GetConfig where

  schema = gSchema

instance ToJSON Cfg where

  toJSON = gtoJson

instance FromJSON Cfg where

  parseJSON = gparseJson

instance JSONSchema Cfg where

  schema = gSchema

instance ToJSON UpstreamCfg where

  toJSON = gtoJson

instance FromJSON UpstreamCfg where

  parseJSON = gparseJson

instance JSONSchema UpstreamCfg where

  schema = gSchema

instance JSONSchema DaemonVerbosity where

  schema = gSchema

instance JSONSchema ContainerRuntime where

  schema = gSchema

instance JSONSchema DownstreamCfg where

  schema = gSchema

instance JSONSchema RaplCfg where

  schema = gSchema

instance JSONSchema HwmonCfg where

  schema = gSchema
