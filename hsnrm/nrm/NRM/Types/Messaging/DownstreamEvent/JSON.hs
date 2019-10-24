{-# OPTIONS_GHC -fno-warn-partial-fields #-}

-- |
-- Module      : NRM.Types.Messaging.DownstreamEvent.JSON
-- Copyright   : (c) UChicago Argonne, 2019
-- License     : BSD3
-- Maintainer  : fre@freux.fr
module NRM.Types.Messaging.DownstreamEvent.JSON
  ( Event (..),
  )
where

import Codegen.CHeader
import Protolude

data Event
  = CmdPerformance
      { cmdID :: Text,
        perf :: Int
      }
  | CmdPause
      { cmdID :: Text
      }
  | ThreadProgress
      { cmdID :: Text,
        processID :: Int,
        taskID :: Text,
        threadID :: Int,
        payload :: Int
      }
  | ThreadPause
      { cmdID :: Text,
        processID :: Int,
        taskID :: Text,
        threadID :: Int
      }
  | ThreadPhaseContext
      { cmdID :: Text,
        processID :: Int,
        taskID :: Text,
        threadID :: Int,
        cpu :: Int,
        aggregation :: Int,
        computetime :: Int,
        totaltime :: Int
      }
  | ThreadPhasePause
      { cmdID :: Text,
        processID :: Int,
        taskID :: Text,
        threadID :: Int
      }
  deriving (Generic, CHeaderGen)
