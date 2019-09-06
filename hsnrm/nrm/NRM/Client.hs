{-|
Module      : NRM.Client
Copyright   : (c) 2019, UChicago Argonne, LLC.
License     : BSD3
Maintainer  : fre@freux.fr
-}
module NRM.Client
  ( main
  , client
  , reqrep
  , dispatchProtocol
  )
where

{-import qualified NRM.Types.Messaging.UpstreamReq as -}
import qualified CPD.Core as CPDC
import qualified CPD.Text as CPD
import Data.Aeson.Encode.Pretty as AP (encodePretty)
import qualified Data.ByteString as SB
import Data.Restricted
import NRM.Classes.Messaging
import NRM.Optparse
import qualified NRM.Optparse.Client as C
import qualified NRM.Types.Cmd as Cmd
import qualified NRM.Types.Messaging.Protocols as Protocols
import qualified NRM.Types.Messaging.UpstreamRep as URep
import qualified NRM.Types.Messaging.UpstreamReq as UReq
import qualified NRM.Types.Slice as C
import NRM.Types.State
import qualified NRM.Types.UpstreamClient as UC
import NeatInterpolation
import Protolude hiding (Rep)
import System.IO (hFlush)
import qualified System.Posix.Signals as SPS
  ( Handler (..)
  , installHandler
  , keyboardSignal
  )
import qualified System.ZMQ4.Monadic as ZMQ
import Text.Pretty.Simple

address :: Text
address = "tcp://localhost:3456"

-- | The main user facing nrm client process
main :: IO ()
main = do
  (C.Opts req common) <- parseClientCli
  when (C.verbose common == C.Verbose) (print $ encode req)
  uuid <-
    UC.nextUpstreamClientID <&> \case
      Nothing -> panic "couldn't generate next client ID"
      Just c -> (restrict (toS $ UC.toText c) :: Restricted (N1, N254) SB.ByteString)
  ZMQ.runZMQ $ do
    s <- ZMQ.socket ZMQ.Dealer
    combine uuid s
    client s req common

client
  :: ZMQ.Socket z ZMQ.Dealer
  -> UReq.Req
  -> C.CommonOpts
  -> ZMQ.ZMQ z ()
client s req v = do
  ZMQ.send s [] (toS $ encode req)
  dispatchProtocol s v req

dispatchProtocol
  :: ZMQ.Socket z ZMQ.Dealer
  -> C.CommonOpts
  -> UReq.Req
  -> ZMQ.ZMQ z ()
dispatchProtocol s v = \case
  (UReq.ReqSliceList x) -> reqrep s v Protocols.SliceList x
  (UReq.ReqKillSlice x) -> reqrep s v Protocols.KillSlice x
  (UReq.ReqKillCmd x) -> reqrep s v Protocols.KillCmd x
  (UReq.ReqSetPower x) -> reqrep s v Protocols.SetPower x
  (UReq.ReqGetConfig x) -> reqrep s v Protocols.GetConfig x
  (UReq.ReqGetState x) -> reqrep s v Protocols.GetState x
  (UReq.ReqCPD x) -> reqrep s v Protocols.CPD x
  (UReq.ReqRun x) ->
    if UReq.detachCmd x
    then putText "Client detached."
    else reqstream s v Protocols.Run x

reqrep
  :: ZMQ.Socket z ZMQ.Dealer
  -> C.CommonOpts
  -> Protocols.ReqRep req rep
  -> req
  -> ZMQ.ZMQ z ()
reqrep s opts = \case
  Protocols.CPD ->
    const $ do
      ZMQ.receive s <&> decode . toS >>= \case
        Nothing -> putText "Couldn't decode reply"
        Just (URep.RepCPD (URep.CPD cpd)) ->
          putText
            [text|
              $actuatorcount actuator(s) currently registered.
              $sensorcount sensor(s) currently registered.
              CPD Internal view
               $cpdT
              CPD Dhall view
               $cpdTD
             |]
          where
            cpdTD = CPD.showProblemDhall cpd
            cpdT = show cpd
            actuatorcount = show $ length (CPDC.actuators cpd)
            sensorcount = show $ length (CPDC.sensors cpd)
        _ -> putText "reply wasn't in protocol"
      liftIO $ hFlush stdout
  Protocols.SliceList ->
    const $ do
      ZMQ.receive s <&> decode . toS >>= \case
        Nothing -> putText "Couldn't decode reply"
        Just (URep.RepList (URep.SliceList l)) ->
          putText
            [text|
              $slicecount slice(s) currently running.
               $slices
             |]
          where
            slicecount = show $ length l
            slices = showSliceList l
        _ -> putText "reply wasn't in protocol"
      liftIO $ hFlush stdout
  Protocols.GetState ->
    const $ do
      msg <- ZMQ.receive s <&> toS
      case decode msg of
        Nothing -> putText "Couldn't decode reply"
        Just (URep.RepGetState (URep.GetState st)) ->
          if C.jsonPrint opts
          then putText $ toS (AP.encodePretty st)
          else
            do
              putText "Current daemon state:"
              pPrint st
        _ -> putText "reply wasn't in protocol"
      liftIO $ hFlush stdout
  Protocols.GetConfig ->
    const $ do
      ZMQ.receive s <&> decode . toS >>= \case
        Nothing -> putText "Couldn't decode reply"
        Just (URep.RepGetConfig (URep.GetConfig cfg)) ->
          if C.jsonPrint opts
          then putText $ toS (AP.encodePretty cfg)
          else
            do
              putText "Daemon configuration:"
              pPrint cfg
        _ -> putText "reply wasn't in protocol"
      liftIO $ hFlush stdout
  Protocols.SetPower ->
    const $ do
      msg <- ZMQ.receive s
      liftIO . print $ ((decode $ toS msg) :: Maybe URep.Rep)
      liftIO $ hFlush stdout
  Protocols.KillSlice ->
    const $ do
      ZMQ.receive s <&> decode . toS >>= \case
        Nothing -> putText "Couldn't decode reply"
        Just (URep.RepSliceKilled (URep.SliceKilled sliceID)) ->
          putText $ "Killed slice ID: " <> C.toText sliceID
        Just (URep.RepNoSuchSlice URep.NoSuchSlice) ->
          putText "No such slice ID. "
        _ -> putText "reply wasn't in protocol"
      liftIO $ hFlush stdout
  Protocols.KillCmd ->
    const $ do
      ZMQ.receive s <&> decode . toS >>= \case
        Nothing -> putText "Couldn't decode reply"
        Just (URep.RepCmdKilled (URep.CmdKilled cmdID)) ->
          putText $ "Killed cmd ID: " <> Cmd.toText cmdID
        Just (URep.RepSliceKilled (URep.SliceKilled sliceID)) ->
          putText $ "Killed slice ID: " <> C.toText sliceID
        Just (URep.RepNoSuchCmd URep.NoSuchCmd) ->
          putText "No such command ID. "
        _ -> putText "reply wasn't in protocol"
      liftIO $ hFlush stdout

reqstream
  :: ZMQ.Socket z ZMQ.Dealer
  -> C.CommonOpts
  -> Protocols.ReqStream req rep
  -> req
  -> ZMQ.ZMQ z ()
reqstream s c Protocols.Run UReq.Run {..} = do
  ZMQ.connect s $ toS address
  msg <- ZMQ.receive s
  case ((decode $ toS msg) :: Maybe URep.Rep) of
    Nothing -> putText "error: received malformed message(1)."
    Just (URep.RepStart (URep.Start _ cmdID)) -> zmqCCHandler (kill cmdID) >> go
    Just (URep.RepStartFailure _) -> putText "Command start failure."
    _ -> putText "NRM client error: received wrong type of message when starting job."
  where
    go = do
      msg <- ZMQ.receive s
      when (C.verbose c == C.Verbose) $ liftIO $ print msg
      case ((decode $ toS msg) :: Maybe URep.Rep) of
        Just (URep.RepStdout (URep.stdoutPayload -> x)) -> putStr x >> go
        Just (URep.RepStderr (URep.stderrPayload -> x)) -> hPutStr stderr x >> go
        Just (URep.RepThisCmdKilled _) -> putText "Command killed."
        Just (URep.RepCmdEnded (URep.exitCode -> x)) -> case x of
          ExitSuccess -> putText "Command ended successfully."
          ExitFailure exitcode -> liftIO $ die ("Command ended with exit code" <> show exitcode)
        Nothing -> putText "error: NRM client received malformed message."
        _ -> putText "error: NRM client received wrong type of message."
    zmqCCHandler :: IO () -> ZMQ.ZMQ z ()
    zmqCCHandler h = void $ liftIO $ SPS.installHandler SPS.keyboardSignal (SPS.CatchOnce h) Nothing

kill :: Cmd.CmdID -> IO ()
kill cmdID =
  ZMQ.runZMQ $ do
    s <- ZMQ.socket ZMQ.Dealer
    uuid <-
      liftIO $ UC.nextUpstreamClientID <&> \case
        Nothing -> panic "couldn't generate next client ID"
        Just c -> (restrict (toS $ UC.toText c) :: Restricted (N1, N254) SB.ByteString)
    combine uuid s
    ZMQ.send s [] (toS $ encode $ UReq.ReqKillCmd (UReq.KillCmd cmdID))

combine :: Restricted (N1, N254) ByteString -> ZMQ.Socket z t -> ZMQ.ZMQ z ()
combine uuid s = do
  ZMQ.setIdentity uuid s
  ZMQ.setSendHighWM (restrict (0 :: Int)) s
  ZMQ.setReceiveHighWM (restrict (0 :: Int)) s
  ZMQ.connect s $ toS address
