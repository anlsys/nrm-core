{-|
Module      : Nrm.Behavior
Description : Nrm Behavior
Copyright   : (c) UChicago Argonne, 2019
License     : BSD3
Maintainer  : fre@freux.fr
-}
module Nrm.Behavior
  ( behavior
  , Behavior (..)
  )
where

import Nrm.Types.Application
{-import Nrm.Types.Configuration-}
import Nrm.Types.NrmState
import Protolude

data SendAPI = UpstreamPub | UpstreamRep

data RecvAPI = DownstreamEvent | UpstreamReq

data Behavior = NoBehavior | Send SendAPI ByteString | StartChild Command Arguments

data NrmEvent = Recv RecvAPI ByteString | DoSensor | DoControl | DoShutdown | DoChildren

behavior :: NrmEvent -> NrmState -> IO (NrmState, Behavior)
behavior (Recv DownstreamEvent s) = return . downstreamReceive s
behavior (Recv UpstreamReq s) = return . upstreamReceive s
behavior DoSensor = doSensor
behavior DoControl = doControl
behavior DoShutdown = doShutdown
behavior DoChildren = doChildren

downstreamReceive :: ByteString -> NrmState -> (NrmState, Behavior)
downstreamReceive _message nrmState = (nrmState, NoBehavior)

upstreamReceive :: ByteString -> NrmState -> (NrmState, Behavior)
upstreamReceive _message nrmState = (nrmState, NoBehavior)

doSensor :: NrmState -> IO (NrmState, Behavior)
doSensor nrmState = return (nrmState, NoBehavior)

doControl :: NrmState -> IO (NrmState, Behavior)
doControl nrmState = return (nrmState, NoBehavior)

doChildren :: NrmState -> IO (NrmState, Behavior)
doChildren nrmState = return (nrmState, NoBehavior)

doShutdown :: NrmState -> IO (NrmState, Behavior)
doShutdown nrmState = return (nrmState, NoBehavior)
