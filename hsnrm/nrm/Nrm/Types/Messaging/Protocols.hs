{-|
Module      : Nrm.Types.Messaging.Protocols
Copyright   : (c) UChicago Argonne, 2019
License     : BSD3
Maintainer  : fre@freux.fr
-}
module Nrm.Types.Messaging.Protocols
  ( ReqRep (..)
  , ReqStream (..)
  )
where

import qualified Nrm.Types.Messaging.UpstreamRep as Rep
import qualified Nrm.Types.Messaging.UpstreamReq as Req

data ReqRep req rep where
  ContainerList :: ReqRep Req.ContainerList Rep.ContainerList
  GetState :: ReqRep Req.GetState Rep.GetState
  GetConfig :: ReqRep Req.GetConfig Rep.GetConfig
  SetPower :: ReqRep Req.SetPower Rep.GetPower
  KillContainer :: ReqRep Req.KillContainer Rep.ContainerDeath
  KillCmd :: ReqRep Req.KillCmd Rep.CmdDeath

data ReqStream req reps where
  Run :: ReqStream Req.Run '[Rep.Stdout, Rep.Stderr, Rep.EndStream]
