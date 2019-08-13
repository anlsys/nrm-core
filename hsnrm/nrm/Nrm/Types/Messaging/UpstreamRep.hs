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

import Nrm.Classes.Messaging
import qualified Nrm.Types.Messaging.UpstreamRep.JSON as J
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
      { stdoutContainerUUUID :: Text
      , stdoutPayload :: Text
      }
  deriving (Show, Generic)

data Stderr
  = Stderr
      { stderrContainerUUID :: Text
      , stderrPayload :: Text
      }
  deriving (Show, Generic)

data Start
  = Start
      { startContainerUUID :: Text
      , pid :: Int
      }
  deriving (Show, Generic)

data ProcessExit
  = ProcessExit
      { container_uuid :: Text
      , status :: Text
      }
  deriving (Show, Generic)

newtype GetPower
  = GetPower
      { limit :: Text
      }
  deriving (Show, Generic)

instance JSONLayer Rep J.Rep where
