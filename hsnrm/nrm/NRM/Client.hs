-- |
-- Module      : NRM.Client
-- Copyright   : (c) 2019, UChicago Argonne, LLC.
-- License     : BSD3
-- Maintainer  : fre@freux.fr
module NRM.Client
  ( main,
    reqrep,
    processReq,
    dispatchProtocol,
    connectWithOptions,
    pubAddress,
    rpcAddress,
  )
where

import qualified CPD.Core as CPD
import CPD.Core (prettyCPD)
import qualified Data.Aeson as A
import Data.Aeson.Encode.Pretty as AP (encodePretty)
import qualified Data.ByteString as BS
import Data.Restricted
import NRM.Classes.Messaging
import NRM.Optparse
import qualified NRM.Optparse.Client as C
import NRM.Types.CmdID as CmdID
import qualified NRM.Types.Messaging.Protocols as Protocols
import NRM.Types.Messaging.UpstreamPub as UPub
import qualified NRM.Types.Messaging.UpstreamRep as URep
import qualified NRM.Types.Messaging.UpstreamReq as UReq
import qualified NRM.Types.Slice as C
import NRM.Types.State
import qualified NRM.Types.UpstreamClient as UC
import NeatInterpolation
import Protolude hiding (Rep)
import System.Exit (exitWith)
import qualified System.Posix.Signals as SPS
  ( Handler (..),
    installHandler,
    keyboardSignal,
  )
import qualified System.ZMQ4.Monadic as ZMQ
import Text.Pretty.Simple

pubAddress :: C.CommonOpts -> Text
pubAddress c = "tcp://" <> C.upstreamBindAddress c <> ":" <> show (C.pubPort c)

rpcAddress :: C.CommonOpts -> Text
rpcAddress c = "tcp://" <> C.upstreamBindAddress c <> ":" <> show (C.rpcPort c)

-- | The main user facing nrm client process
main :: IO ()
main =
  parseClientCli >>= \(C.Opts what common) ->
    what & \case
      Left l ->
        ZMQ.runZMQ $ do
          s <- ZMQ.socket ZMQ.Sub
          ZMQ.setReceiveHighWM (restrict (0 :: Int)) s
          ZMQ.subscribe s BS.empty
          ZMQ.connect s $ toS (pubAddress common)
          subClient l s common
      Right req -> processReq common req

processReq :: C.CommonOpts -> UReq.Req -> IO ()
processReq common req = do
  when (C.verbose common == C.Verbose) (print $ encode req)
  uuid <-
    UC.nextUpstreamClientID <&> \case
      Nothing -> panic "couldn't generate next client ID"
      Just c ->
        restrict (toS $ UC.toText c) ::
          Restricted (N1, N254) BS.ByteString
  ZMQ.runZMQ $ do
    s <- ZMQ.socket ZMQ.Dealer
    connectWithOptions uuid common s
    reqrepClient s req common

subClient ::
  C.Listen ->
  ZMQ.Socket z ZMQ.Sub ->
  C.CommonOpts ->
  ZMQ.ZMQ z ()
subClient l s common =
  forever $ do
    m <- ZMQ.receive s
    decode (toS m) & \case
      Nothing -> putText ("Couldn't decode published message: " <> toS m)
      Just message ->
        filterCPD l message & \case
          Nothing -> return ()
          Just filtered -> liftIO . putText . (if C.jsonPrint common then encodeT else toS . pShow) $ filtered

filterCPD :: C.Listen -> UPub.Pub -> Maybe UPub.Pub
filterCPD C.All p = Just p
filterCPD C.CPDOnly p@PubCPD {} = Just p
filterCPD C.CPDOnly p@PubMeasurements {} = Just p
filterCPD C.CPDOnly p@PubAction {} = Just p
filterCPD C.CPDOnly _ = Nothing
filterCPD C.Raw PubMeasurements {} = Nothing
filterCPD C.Raw PubCPD {} = Nothing
filterCPD C.Raw p = Just p

reqrepClient ::
  ZMQ.Socket z ZMQ.Dealer ->
  UReq.Req ->
  C.CommonOpts ->
  ZMQ.ZMQ z ()
reqrepClient s req c = do
  ZMQ.send s [] (toS $ encode req)
  dispatchProtocol s c req

dispatchProtocol ::
  ZMQ.Socket z ZMQ.Dealer ->
  C.CommonOpts ->
  UReq.Req ->
  ZMQ.ZMQ z ()
dispatchProtocol s c = \case
  (UReq.ReqSliceList x) -> reqrep s c Protocols.SliceList x
  (UReq.ReqKillSlice x) -> reqrep s c Protocols.KillSlice x
  (UReq.ReqKillCmd x) -> reqrep s c Protocols.KillCmd x
  (UReq.ReqActuate x) -> reqrep s c Protocols.Actuate x
  (UReq.ReqGetConfig x) -> reqrep s c Protocols.GetConfig x
  (UReq.ReqGetState x) -> reqrep s c Protocols.GetState x
  (UReq.ReqCPD x) -> reqrep s c Protocols.CPD x
  (UReq.ReqRun x) ->
    if UReq.detachCmd x
      then putText "Client detached."
      else reqstream s c Protocols.Run x

pShowOpts :: (Show a, A.ToJSON a) => C.CommonOpts -> a -> Text
pShowOpts opts = if C.jsonPrint opts then toS . AP.encodePretty else pShowColor (C.color opts)
  where
    pShowColor :: (Show a) => Bool -> a -> Text
    pShowColor True = toS . pShow
    pShowColor False = toS . pShowNoColor

reqrep ::
  ZMQ.Socket z ZMQ.Dealer ->
  C.CommonOpts ->
  Protocols.ReqRep req rep ->
  req ->
  ZMQ.ZMQ z ()
reqrep s opts proto =
  const $
    ZMQ.receive s <&> (decodeT :: Text -> Maybe URep.Rep) . toS >>= \case
      Nothing -> putText "Couldn't decode reply"
      Just rep -> (proto, rep) & \case
        (Protocols.CPD, URep.RepCPD cpd) ->
          putText
            [text|
                asynchronous Control Problem Description:
                $cpdT

                $actuatorcount actuator(s) currently registered.
                $sensorcount sensor(s) currently registered.
               |]
          where
            cpdT = prettyCPD cpd
            actuatorcount = show $ length (CPD.actuators cpd)
            sensorcount = show $ length (CPD.sensors cpd)
        (Protocols.SliceList, URep.RepList (URep.SliceList l)) ->
          putText
            [text|
                      $slicecount slice(s) currently running.
                       $slices
                     |]
          where
            slicecount = show $ length l
            slices = showSliceList l
        (Protocols.GetState, URep.RepGetState st) ->
          putText $ pShowOpts opts st
        (Protocols.GetConfig, URep.RepGetConfig (URep.GetConfig cfg)) ->
          if C.jsonPrint opts
            then putText $ toS (AP.encodePretty cfg)
            else putText $ pShowOpts opts cfg
        (Protocols.Actuate, URep.RepActuate URep.Actuated) ->
          putText "actuated"
        (Protocols.Actuate, URep.RepActuate URep.NotActuated) ->
          putText "couldn't actuate"
        (Protocols.KillSlice, URep.RepSliceKilled (URep.SliceKilled sliceID)) ->
          putText $ "Killed slice ID: " <> C.toText sliceID
        (Protocols.KillSlice, URep.RepNoSuchSlice URep.NoSuchSlice) ->
          putText "No such slice ID. "
        (Protocols.KillCmd, URep.RepCmdKilled (URep.CmdKilled cmdID)) ->
          putText $ "Killed cmd ID: " <> CmdID.toText cmdID
        (Protocols.KillCmd, URep.RepSliceKilled (URep.SliceKilled sliceID)) ->
          putText $ "Killed slice ID: " <> C.toText sliceID
        (Protocols.KillCmd, URep.RepNoSuchCmd URep.NoSuchCmd) ->
          putText "No such command ID. "
        (_, URep.RepException e) -> putText $ "daemon throws internal exception: \n" <> e
        _ -> putText "reply wasn't in protocol"

reqstream ::
  ZMQ.Socket z ZMQ.Dealer ->
  C.CommonOpts ->
  Protocols.ReqStream req rep ->
  req ->
  ZMQ.ZMQ z ()
reqstream s c Protocols.Run UReq.Run {..} = do
  ZMQ.connect s $ toS (rpcAddress c)
  msg <- ZMQ.receive s
  case ((decodeT $ toS msg) :: Maybe URep.Rep) of
    Nothing -> putText "error: received malformed message(1)."
    Just (URep.RepStart (URep.Start _ cmdID)) -> zmqCCHandler (kill cmdID c) >> go
    Just (URep.RepStartFailure _) -> putText "Command start failure."
    _ -> putText "NRM client error: received wrong type of message when starting job."
  where
    go = do
      msg <- ZMQ.receive s
      when (C.verbose c == C.Verbose) $ liftIO $ print msg
      case ((decodeT $ toS msg) :: Maybe URep.Rep) of
        Just (URep.RepStdout (URep.stdoutPayload -> x)) -> putStr x >> go
        Just (URep.RepStderr (URep.stderrPayload -> x)) -> hPutStr stderr x >> go
        Just (URep.RepThisCmdKilled _) -> putText "Command killed."
        Just (URep.RepCmdEnded (URep.exitCode -> x)) -> case x of
          ExitSuccess -> putText "Command ended successfully."
          ec@(ExitFailure exitcode) ->
            liftIO $
              putText ("Command ended with exit code: " <> show (exitcode `mod` 256))
                >> exitWith ec
        Nothing -> putText "error: NRM client received malformed message."
        _ -> putText "error: NRM client received wrong type of message."
    zmqCCHandler :: IO () -> ZMQ.ZMQ z ()
    zmqCCHandler h = void $ liftIO $ SPS.installHandler SPS.keyboardSignal (SPS.CatchOnce h) Nothing

kill :: CmdID -> C.CommonOpts -> IO ()
kill cmdID c =
  ZMQ.runZMQ $ do
    s <- ZMQ.socket ZMQ.Dealer
    uuid <-
      liftIO $
        UC.nextUpstreamClientID <&> \case
          Nothing -> panic "couldn't generate next client ID"
          Just clientID -> (restrict (toS $ UC.toText clientID) :: Restricted (N1, N254) BS.ByteString)
    connectWithOptions uuid c s
    ZMQ.send s [] (toS $ encode $ UReq.ReqKillCmd (UReq.KillCmd cmdID))

connectWithOptions :: Restricted (N1, N254) ByteString -> C.CommonOpts -> ZMQ.Socket z t -> ZMQ.ZMQ z ()
connectWithOptions uuid c s = do
  ZMQ.setIdentity uuid s
  ZMQ.setSendHighWM (restrict (0 :: Int)) s
  ZMQ.setReceiveHighWM (restrict (0 :: Int)) s
  ZMQ.connect s $ toS (rpcAddress c)
