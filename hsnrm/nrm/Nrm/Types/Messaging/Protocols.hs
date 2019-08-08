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
  ContainerList :: ReqRep Req.ContainerListRequest Rep.ContainerList
  SetPower :: ReqRep Req.SetPowerRequest Rep.GetPower
  Kill :: ReqRep Req.KillRequest Rep.ProcessExit

data ReqStream req reps where
  Run :: ReqStream Req.RunRequest '[Rep.Stdout, Rep.Stderr, Rep.ProcessExit]
