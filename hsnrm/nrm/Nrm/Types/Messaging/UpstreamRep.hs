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
  , ProcessExit (..)
  , GetPower (..)
  )
where

import Data.Aeson
import Data.MessagePack
import Data.JSON.Schema
import Generics.Generic.Aeson
import Nrm.Classes.Messaging
import Nrm.Types.Container as C
import Nrm.Types.Process as P
import Nrm.Types.Units as U
import Protolude hiding (Rep)

data Rep
  = RepList ContainerList
  | RepStdout Stdout
  | RepStderr Stderr
  | RepStart Start
  | RepProcessExit ProcessExit
  | RepGetPower GetPower
  deriving (Show, Generic)

newtype ContainerList
  = ContainerList
      { containers :: [Text]
      }
  deriving (Show, Generic)

data Stdout
  = Stdout
      { stdoutContainerUUUID :: C.ContainerID
      , stdoutPayload :: Text
      }
  deriving (Show, Generic)

data Stderr
  = Stderr
      { stderrContainerID :: C.ContainerID
      , stderrPayload :: Text
      }
  deriving (Show, Generic)

data Start
  = Start
      { startContainerID :: C.ContainerID
      , pid :: P.ProcessID
      }
  deriving (Show, Generic)

data ProcessExit
  = ProcessExit
      { container_uuid :: Text
      , status :: Int
      }
  deriving (Show, Generic)

newtype GetPower
  = GetPower
      { limit :: U.Power
      }
  deriving (Show, Generic)

instance NrmMessage Rep Rep where

  fromJ = identity

  toJ = identity

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

instance ToJSON ProcessExit where

  toJSON = gtoJson

instance FromJSON ProcessExit where

  parseJSON = gparseJson

instance JSONSchema ProcessExit where

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

deriving instance MessagePack Rep
deriving instance MessagePack ContainerList
deriving instance MessagePack Stdout
deriving instance MessagePack Stderr
deriving instance MessagePack Start
deriving instance MessagePack ProcessExit
deriving instance MessagePack GetPower
