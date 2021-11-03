-- |
-- Module      : NRM.Types.Messaging.Protocols
-- Copyright   : (c) UChicago Argonne, 2019
-- License     : BSD3
-- Maintainer  : swann@anl.gov
module NRM.Types.Messaging.Protocols
  ( ReqRep (..),
    ReqStream (..),
  )
where

import qualified CPD.Core as CPD
import qualified CPD.Values as CPD
import qualified NRM.Types.Messaging.UpstreamRep as Rep
import qualified NRM.Types.Messaging.UpstreamReq as Req
import qualified NRM.Types.State

data ReqRep req rep where
  SliceList :: ReqRep Req.SliceList Rep.SliceList
  GetState :: ReqRep Req.GetState NRM.Types.State.NRMState
  GetConfig :: ReqRep Req.GetConfig Rep.GetConfig
  Actuate :: ReqRep [CPD.Action] Rep.Actuated
  KillSlice :: ReqRep Req.KillSlice Rep.SliceKilled
  KillCmd :: ReqRep Req.KillCmd Rep.CmdKilled
  CPD :: ReqRep Req.CPD CPD.Problem

data ReqStream req reps where
  Run :: ReqStream Req.Run '[Rep.Stdout, Rep.Stderr, Rep.CmdEnded]
