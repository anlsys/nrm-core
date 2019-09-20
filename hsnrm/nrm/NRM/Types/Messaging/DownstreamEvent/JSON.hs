{-# OPTIONS_GHC -fno-warn-partial-fields #-}
{-|
Module      : NRM.Types.Messaging.DownstreamEvent.JSON
Copyright   : (c) UChicago Argonne, 2019
License     : BSD3
Maintainer  : fre@freux.fr
-}
module NRM.Types.Messaging.DownstreamEvent.JSON
  ( Event (..)
  )
where

import Codegen.CHeader
import Protolude

data Event
  = CmdStart {cmdID :: Text}
  | CmdPerformance
      { cmdID :: Text
      , perf :: Int
      }
  | CmdExit {cmdID :: Text}
  | ThreadStart
      { cmdID :: Text
      , processID :: Text
      , taskID :: Text
      , threadID :: Text
      }
  | ThreadProgress
      { cmdID :: Text
      , processID :: Text
      , taskID :: Text
      , threadID :: Text
      , payload :: Int
      }
  | ThreadPhaseContext
      { cmdID :: Text
      , processID :: Text
      , taskID :: Text
      , threadID :: Text
      , cpu :: Int
      , startcompute :: Int
      , endcompute :: Int
      , startbarrier :: Int
      , endbarrier :: Int
      }
  | ThreadExit
      { cmdID :: Text
      , processID :: Text
      , taskID :: Text
      , threadID :: Text
      }
  deriving (Generic, CHeaderGen)
