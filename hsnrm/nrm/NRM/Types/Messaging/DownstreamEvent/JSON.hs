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
  = CmdPerformance
      { cmdID :: Text
      , timestamp :: Double
      , perf :: Int
      }
  | CmdPause
      { cmdID :: Text
      , timestamp :: Double
      }
  | ThreadProgress
      { cmdID :: Text
      , processID :: Int
      , taskID :: Int
      , threadID :: Int
      , payload :: Int
      }
  | ThreadPause
      { cmdID :: Text
      , processID :: Int
      , taskID :: Int
      , threadID :: Int
      }
--  | ThreadPhaseContext
--      { cmdID :: Text
--      , processID :: Int
--      , taskID :: Int
--      , threadID :: Int
--      , cpu :: Int
--      , startcompute :: Int
--      , endcompute :: Int
--      , startbarrier :: Int
--      , endbarrier :: Int
--      }
--  | ThreadPhasePause
--      { cmdID :: Text
--      , processID :: Int
--      , taskID :: Int
--      , threadID :: Int
--      }
  deriving (Generic, CHeaderGen)
