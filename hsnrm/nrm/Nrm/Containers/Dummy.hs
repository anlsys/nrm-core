{-|
Module      : Nrm.Containers.Dummy
Copyright   : (c) 2019, UChicago Argonne, LLC.
License     : BSD3
Maintainer  : fre@freux.fr

Information management for an exec-based dummy container runtime.

-}
module Nrm.Containers.Dummy
  ( Dummy (..)
  , DummyRuntime
  , emptyRuntime
  )
where

import Data.Aeson
import Data.Map
import Data.MessagePack
import Nrm.Containers.Class
import Nrm.Processes
import Nrm.Types.Container
import Protolude
import qualified System.Posix.Signals as Signals

type DummyRuntime = Dummy (Map ContainerID [ApplicationProcess])

newtype Dummy a = Dummy a
  deriving (Show, Generic, Functor, MessagePack, ToJSON, FromJSON)

emptyRuntime :: Dummy (Map ContainerID a)
emptyRuntime = Dummy $ fromList []

instance (MonadIO m) => ContainerRuntime m DummyRuntime () () where

  doEnableRuntime _ = return $ Right emptyRuntime

  doDisableRuntime (Dummy m) = do
    for_ m $ mapM_ killIfRegistered
    return $ Right emptyRuntime
    where
      killIfRegistered (Registered _ pid) = liftIO $ signalProcess Signals.sigKILL pid
      killIfRegistered (Unregistered _) = return ()

  doCreateContainer runtime () =
    liftIO $ nextContainerID <&> \case
      Just uuid -> Right (insert uuid [] <$> runtime, uuid)
      Nothing -> Left "Failure to generate next Container ID"

  doPrepareStartApp runtime containerID AppStartConfig {..} =
    return $
      Right
        ( adjust (Unregistered cmdID :) containerID <$> runtime
        , command
        , arguments
        )

  doStopContainer (Dummy x) containerID =
    case lookup containerID x of
      Nothing -> return $ Left "Unknown container ID"
      Just dals -> do
        let pids = catMaybes $ go <$> dals
        for_ pids $ liftIO . signalProcess Signals.sigKILL
        return $ Right $ Dummy $ delete containerID x
    where
      go (Registered _ pid) = Just pid
      go (Unregistered _) = Nothing

  listContainers (Dummy l) = keys l

  registerStartApp runtime containerID cmdID pid =
    adjust (go <$>) containerID <$> runtime
    where
      go x
        | x == Unregistered cmdID = Registered cmdID pid
        | otherwise = x

  registerStopApp runtime (Left processID) = Data.Map.map (Protolude.filter f) <$> runtime
    where
      f (Registered _ pid) = pid == processID
      f (Unregistered _) = False
  registerStopApp runtime (Right cmdID) = Data.Map.map (Protolude.filter f) <$> runtime
    where
      f (Registered appid _) = appid == cmdID
      f (Unregistered appid) = appid == cmdID

  listApplications (Dummy runtime) containerID = lookup containerID runtime
