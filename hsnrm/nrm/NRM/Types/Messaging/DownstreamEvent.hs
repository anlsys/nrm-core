{-|
Module      : NRM.Types.Messaging.DownstreamEvent
Copyright   : (c) UChicago Argonne, 2019
License     : BSD3
Maintainer  : fre@freux.fr
-}
module NRM.Types.Messaging.DownstreamEvent
  ( Event (..)
  , Progress (..)
  , PhaseContext (..)
  , Performance (..)
  )
where

import Data.MessagePack
import qualified NRM.Classes.Messaging as M
import qualified NRM.Types.DownstreamClient as D
import qualified NRM.Types.Messaging.DownstreamEvent.JSON as J
import qualified NRM.Types.Units as U
import Protolude

data Event
  = ThreadStart D.DownstreamThreadID
  | ThreadProgress D.DownstreamThreadID Progress
  | ThreadPhaseContext D.DownstreamThreadID PhaseContext
  | ThreadExit D.DownstreamThreadID
  | CmdStart D.DownstreamCmdID
  | CmdPerformance D.DownstreamCmdID Performance
  | CmdExit D.DownstreamCmdID
  deriving (Generic, MessagePack)

newtype Performance
  = Performance
      { perf :: U.Operations
      }
  deriving (Generic, MessagePack)

newtype Progress
  = Progress
      { payload :: U.Progress
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

  toJ = panic "need to implement toJ for DownstreamEvent"

  {-toJ = \case-}
  {-EventStart Start {..} -> J.Start-}
  {-{ slice_uuid = C.toText startSliceID-}
  {-, application_uuid = D.toText startDownstreamID-}
  {-}-}
  {-EventExit Exit {..} -> J.Exit-}
  {-{ application_uuid = D.toText exitDownstreamID-}
  {-}-}
  {-EventPerformance Performance {..} -> J.Performance-}
  {-{ slice_uuid = C.toText performanceSliceID-}
  {-, application_uuid = D.toText performanceDownstreamID-}
  {-, perf = o-}
  {-}-}
  {-where-}
  {-(U.Operations o) = perf-}
  {-EventProgress Progress {..} -> J.Progress-}
  {-{ application_uuid = D.toText progressDownstreamID-}
  {-, payload = p-}
  {-}-}
  {-where-}
  {-(U.Progress p) = payload-}
  {-EventPhaseContext PhaseContext {..} -> J.PhaseContext {..}-}
  fromJ = panic "need to implement toJ for DownstreamEvent"

{-fromJ = \case-}
{-J.Start {..} ->-}
{-EventStart $ Start-}
{-{ startSliceID = C.parseSliceID slice_uuid-}
{-, startDownstreamID = fromMaybe-}
{-(panic "DownstreamEvent fromJ error on Application ID")-}
{-(D.parseDownstreamID application_uuid)-}
{-}-}
{-J.Exit {..} ->-}
{-EventExit $ Exit-}
{-{ exitDownstreamID = fromMaybe-}
{-(panic "DownstreamEvent fromJ error on Application ID")-}
{-(D.parseDownstreamID application_uuid)-}
{-}-}
{-J.Performance {..} ->-}
{-EventPerformance $ Performance-}
{-{ performanceSliceID = C.parseSliceID slice_uuid-}
{-, performanceDownstreamID = fromMaybe-}
{-(panic "DownstreamEvent fromJ error on Application ID")-}
{-(D.parseDownstreamID application_uuid)-}
{-, perf = U.Operations perf-}
{-}-}
{-J.Progress {..} ->-}
{-EventProgress $ Progress-}
{-{ progressDownstreamID = fromMaybe-}
{-(panic "DownstreamEvent fromJ error on Application ID")-}
{-(D.parseDownstreamID application_uuid)-}
{-, payload = U.Progress payload-}
{-}-}
{-J.PhaseContext {..} -> EventPhaseContext PhaseContext {..}-}
