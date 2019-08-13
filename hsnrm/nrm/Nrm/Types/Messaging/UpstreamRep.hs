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
  deriving (Show)

newtype ContainerList
  = ContainerList
      { containers :: [Text]
      }
  deriving (Show)

data Stdout
  = Stdout
      { stdoutContainerUUUID :: Text
      , stdoutPayload :: Text
      }
  deriving (Show)

data Stderr
  = Stderr
      { stderrContainerUUID :: Text
      , stderrPayload :: Text
      }
  deriving (Show)

data Start
  = Start
      { startContainerUUID :: Text
      , pid :: Int
      }
  deriving (Show)

data ProcessExit
  = ProcessExit
      { container_uuid :: Text
      , status :: Text
      }
  deriving (Show)

newtype GetPower
  = GetPower
      { limit :: Text
      }
  deriving (Show)

instance NrmMessage Rep J.Rep
