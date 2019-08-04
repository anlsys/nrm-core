{-# LANGUAGE DeriveFunctor #-}
{-# LANGUAGE NoImplicitPrelude #-}

{-|
Module      : Nrm.Containers.Dummy
Description : Dummy container runtime
Copyright   : (c) 2019, UChicago Argonne, LLC.
License     : BSD3
Maintainer  : fre@freux.fr

Information management for an exec-based dummy container runtime.

-}
module Nrm.Containers.Dummy
  ( Dummy (..)
  , DummyRuntime
  )
where

import Data.Map
import Nrm.Containers.Class
import Nrm.Types.Applications
import Nrm.Types.Containers
import Protolude
import System.Posix.Signals

type DummyRuntime = Dummy (Map ContainerUUID [ApplicationProcess])

newtype Dummy a = Dummy a
  deriving (Functor)

emptyRuntime :: Dummy (Map ContainerUUID a)
emptyRuntime = Dummy $ fromList []

instance (MonadIO m) => ContainerRuntime m DummyRuntime () () where

  doEnableRuntime _ = return $ Right emptyRuntime

  doDisableRuntime (Dummy m) = do
    for_ m $ mapM_ killIfRegistered
    return $ Right emptyRuntime
    where
      killIfRegistered (Registered _ pid) = liftIO $ signalProcess sigKILL pid
      killIfRegistered (Unregistered _) = return ()

  doCreateContainer runtime () =
    liftIO $ nextContainerUUID <&> \case
      Just uuid -> Right (insert uuid [] <$> runtime, uuid)
      Nothing -> Left "Failure to generate next Container UUID"

  doPrepareStartApp runtime containerUUID AppStartConfig {..} =
    return $
      Right
        ( adjust (Unregistered applicationUUID :) containerUUID <$> runtime
        , command
        , arguments
        )

  doStopContainer (Dummy x) containerUUID =
    case lookup containerUUID x of
      Nothing -> return $ Left "Unknown container UUID"
      Just dals -> do
        let pids = catMaybes $ go <$> dals
        for_ pids $ liftIO . signalProcess sigKILL
        return $ Right $ Dummy $ delete containerUUID x
    where
      go (Registered _ pid) = Just pid
      go (Unregistered _) = Nothing

  listContainers (Dummy l) = keys l

  registerStartApp runtime containerUUID applicationUUID pid =
    adjust (go <$>) containerUUID <$> runtime
    where
      go x
        | x == Unregistered applicationUUID = Registered applicationUUID pid
        | otherwise = x

  registerStopApp runtime (Left processID) = Data.Map.map (Protolude.filter f) <$> runtime
    where
      f (Registered _ pid) = pid == processID
      f (Unregistered _) = False
  registerStopApp runtime (Right applicationUUID) = Data.Map.map (Protolude.filter f) <$> runtime
    where
      f (Registered appid _) = appid == applicationUUID
      f (Unregistered appid) = appid == applicationUUID

  listApplications (Dummy runtime) containerUUID = lookup containerUUID runtime
