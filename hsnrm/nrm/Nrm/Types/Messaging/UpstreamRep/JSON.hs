{-|
Module      : Nrm.Types.Messaging.UpstreamRep.JSON
Copyright   : (c) UChicago Argonne, 2019
License     : BSD3
Maintainer  : fre@freux.fr
-}
module Nrm.Types.Messaging.UpstreamRep.JSON
  ( Rep (..)
  )
where

import qualified Data.Aeson as A
import Data.JSON.Schema
import Protolude hiding (Rep)

data Rep
  = List
      { containers :: [Text]
      }
  | Stdout
      { stdoutContainerUUUID :: Text
      , stdoutPayload :: Text
      }
  | Stderr
      { stderrContainerUUID :: Text
      , stderrPayload :: Text
      }
  | Start
      { startContainerUUID :: Text
      , pid :: Int
      }
  | ProcessExit
      { container_uuid :: Text
      , status :: Text
      }
  | GetPower
      { limit :: Text
      }
  deriving (Show, Generic)
