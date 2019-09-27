{-|
Module      : NRM.Types.Messaging.DownstreamEvent
Copyright   : (c) UChicago Argonne, 2019
License     : BSD3
Maintainer  : fre@freux.fr
-}
module NRM.Types.Messaging.DownstreamEvent
  ( Event (..)
  , CmdHeader (..)
  , ThreadHeader (..)
  , Performance (..)
  , Progress (..)
  , PhaseContext (..)
  )
where

import Data.Maybe (fromJust)
import Data.MessagePack
import qualified NRM.Classes.Messaging as M
import qualified NRM.Types.Cmd as Cmd
import qualified NRM.Types.DownstreamThread as D
import qualified NRM.Types.Messaging.DownstreamEvent.JSON as J
import qualified NRM.Types.Process as P
import qualified NRM.Types.Units as U
import Protolude

data Event
  = CmdPerformance CmdHeader Performance
  | CmdPause CmdHeader
  | ThreadProgress ThreadHeader Progress
  | ThreadPause ThreadHeader
  | ThreadPhaseContext ThreadHeader PhaseContext
  | ThreadPhasePause ThreadHeader
  deriving (Generic, MessagePack)

newtype CmdHeader
  = CmdHeader
      { cmdID :: Cmd.CmdID
      }
  deriving (Generic, MessagePack)

data ThreadHeader
  = ThreadHeader
      { threadCmdID :: Cmd.CmdID
      , processID :: P.ProcessID
      , taskID :: Text
      , threadID :: D.DownstreamThreadID
      }
  deriving (Generic, MessagePack)

newtype Progress
  = Progress
      { progress :: U.Progress
      }
  deriving (Generic, MessagePack)

newtype Performance
  = Performance
      { perf :: U.Operations
      }
  deriving (Generic, MessagePack)

data PhaseContext
  = PhaseContext
      { cpu :: Int
      , startcompute :: Int
      , endcompute :: Int
      , startbarrier :: Int
      , endbarrier :: Int
      }
  deriving (Generic, MessagePack)

instance M.NRMMessage Event J.Event where

  toJ = \case
    CmdPerformance (CmdHeader cmdID) Performance {..} ->
      J.CmdPerformance
        { cmdID = Cmd.toText cmdID
        , perf = U.fromOps perf
        }
    _ -> panic "Non-Cmd downstream API not implemented yet."

  fromJ = \case
    J.CmdPerformance {..} ->
      CmdPerformance (CmdHeader (fromJust $ Cmd.fromText cmdID)) (Performance $ U.Operations perf)
    _ -> panic "Non-Cmd downstream API not implemented yet."

{-CmdPause-}
{-{ cmdID :: Text-}
{-, perf :: Int-}
{-}-}
{-| ThreadProgress-}
{-{ cmdID :: Text-}
{-, processID :: Text-}
{-, taskID :: Text-}
{-, threadID :: Text-}
{-, payload :: Int-}
{-}-}
{-| ThreadPause-}
{-{ cmdID :: Text-}
{-, processID :: Text-}
{-, taskID :: Text-}
{-, threadID :: Text-}
{-}-}
{-| ThreadPhaseContext-}
{-{ cmdID :: Text-}
{-, processID :: Text-}
{-, taskID :: Text-}
{-, threadID :: Text-}
{-, cpu :: Int-}
{-, startcompute :: Int-}
{-, endcompute :: Int-}
{-, startbarrier :: Int-}
{-, endbarrier :: Int-}
{-}-}
{-| ThreadPhasePause-}
{-{ cmdID :: Text-}
{-, processID :: Text-}
{-, taskID :: Text-}
{-, threadID :: Text-}
{-}-}
