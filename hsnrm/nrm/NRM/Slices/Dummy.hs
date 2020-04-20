{-# LANGUAGE DerivingVia #-}

-- |
-- Module      : NRM.Slices.Dummy
-- Copyright   : (c) 2019, UChicago Argonne, LLC.
-- License     : BSD3
-- Maintainer  : fre@freux.fr
--
-- Information management for an exec-based dummy slice runtime.
module NRM.Slices.Dummy
  ( Dummy (..),
    DummyRuntime,
    emptyRuntime,
  )
where

import Data.Aeson
import Data.Data
import Data.Map as M
import Data.MessagePack
import NRM.Processes
import NRM.Slices.Class
import NRM.Types.Slice
import Protolude
import qualified System.Posix.Signals as Signals

type DummyRuntime = Dummy (Map SliceID [ApplicationProcess])

newtype Dummy a = Dummy a
  deriving (Show, Generic, Data, Functor, MessagePack, ToJSON, FromJSON)

emptyRuntime :: Dummy (Map SliceID a)
emptyRuntime = Dummy $ fromList []

instance (MonadIO m) => SliceRuntime m DummyRuntime () () where

  doEnableRuntime _ = return $ Right emptyRuntime

  doDisableRuntime (Dummy m) = do
    for_ m $ mapM_ killIfRegistered
    return $ Right emptyRuntime
    where
      killIfRegistered :: (MonadIO m) => ApplicationProcess -> m ()
      killIfRegistered (Registered _ pid) = liftIO $ signalProcess Signals.sigKILL pid
      killIfRegistered (Unregistered _) = pass

  doCreateSlice runtime () =
    liftIO $
      nextSliceID <&> \case
        Just uuid -> Right (insert uuid [] <$> runtime, uuid)
        Nothing -> Left "Failure to generate next Slice ID"

  doPrepareStartApp runtime sliceID AppStartConfig {..} =
    return $
      Right
        ( M.adjust (Unregistered cmdID :) sliceID <$> runtime,
          command,
          arguments
        )

  doStopSlice (Dummy x) sliceID =
    case lookup sliceID x of
      Nothing -> return $ Left "Unknown slice ID"
      Just dals -> do
        let pids = catMaybes $ go <$> dals
        for_ pids $ liftIO . signalProcess Signals.sigKILL
        return . Right . Dummy $ delete sliceID x
    where
      go (Registered _ pid) = Just pid
      go (Unregistered _) = Nothing

  listSlices (Dummy l) = keys l

  registerStartApp runtime sliceID cmdID pid =
    M.adjust (go <$>) sliceID <$> runtime
    where
      go x
        | x == Unregistered cmdID = Registered cmdID pid
        | otherwise = x

  registerStopApp runtime (Left processID) = M.map (Protolude.filter f) <$> runtime
    where
      f (Registered _ pid) = pid == processID
      f (Unregistered _) = False
  registerStopApp runtime (Right cmdID) = M.map (Protolude.filter f) <$> runtime
    where
      f (Registered appid _) = appid == cmdID
      f (Unregistered appid) = appid == cmdID

  listApplications (Dummy runtime) sliceID = lookup sliceID runtime
