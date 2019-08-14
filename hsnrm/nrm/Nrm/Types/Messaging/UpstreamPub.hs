{-|
Module      : Nrm.Types.Messaging.UpstreamPub
Copyright   : (c) UChicago Argonne, 2019
License     : BSD3
Maintainer  : fre@freux.fr
-}
module Nrm.Types.Messaging.UpstreamPub
  ( Pub (..)
  )
where

import Data.Aeson
import Data.JSON.Schema
import Generics.Generic.Aeson
import Nrm.Classes.Messaging
import Nrm.Types.Application as A
import Nrm.Types.Container as C
import qualified Nrm.Types.Units as U
import Protolude

data Pub
  = PubPower Power
  | PubContainerStart ContainerStart
  | PubContainerExit ContainerExit
  | PubPerformance Performance
  | PubProgress Progress
  | PubControl Control
  deriving (Show, Generic)

data Power
  = Power
      { total :: U.Power
      , limit :: U.Power
      }
  deriving (Show, Generic)

data ContainerStart
  = ContainerStart
      { startContainerUUID :: C.ContainerUUID
      , errno :: Int
      , power :: U.Power
      }
  deriving (Show, Generic)

data ContainerExit
  = ContainerExit
      { exitContainerUUID :: C.ContainerUUID
      , profile_data :: Map Text Text
      }
  deriving (Show, Generic)

data Performance
  = Performance
      { perfContainerUUID :: C.ContainerUUID
      , perf :: U.Operations
      }
  deriving (Show, Generic)

data Progress
  = Progress
      { applicationUUID :: A.ApplicationUUID
      , payload :: U.Progress
      }
  deriving (Show, Generic)

data Control
  = Control
      { powercap :: U.Power
      , energy :: U.Energy
      , performance :: U.Operations
      , control_time :: U.Time
      , feedback_time :: U.Time
      }
  deriving (Show, Generic)

instance ToJSON Control where

  toJSON = gtoJson

instance FromJSON Control where

  parseJSON = gparseJson

instance JSONSchema Control where

  schema = gSchema

instance ToJSON Power where

  toJSON = gtoJson

instance FromJSON Power where

  parseJSON = gparseJson

instance JSONSchema Power where

  schema = gSchema

instance NrmMessage Pub Pub where

  toJ = identity

  fromJ = identity

instance ToJSON Performance where

  toJSON = gtoJson

instance FromJSON Performance where

  parseJSON = gparseJson

instance JSONSchema Performance where

  schema = gSchema

instance ToJSON Progress where

  toJSON = gtoJson

instance FromJSON Progress where

  parseJSON = gparseJson

instance JSONSchema Progress where

  schema = gSchema

instance ToJSON ContainerStart where

  toJSON = gtoJson

instance FromJSON ContainerStart where

  parseJSON = gparseJson

instance JSONSchema ContainerStart where

  schema = gSchema

instance ToJSON ContainerExit where

  toJSON = gtoJson

instance FromJSON ContainerExit where

  parseJSON = gparseJson

instance JSONSchema ContainerExit where

  schema = gSchema
