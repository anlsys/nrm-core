{-|
Module      : NRM.Types.State
Copyright   : (c) UChicago Argonne, 2019
License     : BSD3
Maintainer  : fre@freux.fr
-}
module NRM.Types.State
  ( NRMState (..)
  , -- * Insertion
    insertSlice
  , -- * Useful maps
    cmdIDMap
  , pidMap
  , awaitingCmdIDMap
  , runningCmdIDCmdMap
  , awaitingCmdIDCmdMap
  , -- * Rendering views
    showSliceList
  , showSlices
  )
where

import qualified CPD.Core as CPD
import Control.Lens
import Data.Aeson
import Data.Data
import Data.JSON.Schema
import Data.MessagePack
import NRM.Classes.Sensors
import NRM.Classes.Actuators
import NRM.Slices.Dummy
import NRM.Slices.Nodeos
import NRM.Slices.Singularity
import NRM.Types.Cmd as Cmd
import NRM.Types.LMap as LM
import NRM.Types.Process as P
import NRM.Types.Slice as C
import NRM.Types.Topology
import Protolude

data NRMState
  = NRMState
      { cpd :: CPD.Problem
      , pus :: LMap PUID PU
      , cores :: LMap CoreID Core
      , packages :: LMap PackageID Package
      , slices :: LMap SliceID Slice
      , dummyRuntime :: Maybe DummyRuntime
      , singularityRuntime :: Maybe SingularityRuntime
      , nodeosRuntime :: Maybe NodeosRuntime
      }
  deriving (Show, Generic, Data, MessagePack, ToJSON, FromJSON)

instance Actuators NRMState where

  actuators NRMState {..} =
    actuators pus <>
      actuators cores <>
      actuators packages <>
      actuators slices

instance Sensors NRMState where

  passiveSensors NRMState {..} =
    passiveSensors pus <>
      passiveSensors cores <>
      passiveSensors packages <>
      passiveSensors slices

  activeSensors NRMState {..} =
    activeSensors pus <>
      activeSensors cores <>
      activeSensors packages <>
      activeSensors slices

instance JSONSchema NRMState where

  schema _ = schema (Proxy :: Proxy Text)

showSliceList :: [(SliceID, Slice)] -> Text
showSliceList l =
  mconcat $ l <&> \(sliceID, Slice {..}) ->
    "slice: ID " <> C.toText sliceID <> "\n" <> mconcat (descCmd <$> LM.toList cmds)
  where
    descCmd (cmdID, cmdCore -> CmdCore {..}) =
      " command: ID " <> Cmd.toText cmdID <> descSpec cmdPath arguments <> "\n"
    descSpec
      :: ( IsString a
         , Monoid a
         , StringConv Text a
         , StringConv Arg a
         )
      => Command
      -> Arguments
      -> a
    descSpec (Command cmd) (Arguments args) =
      " : " <> toS cmd <> " " <> (mconcat . intersperse " " $ toS <$> args)

-- | Renders a textual view of running slices
showSlices :: NRMState -> Text
showSlices NRMState {..} =
  showSliceList $ LM.toList slices

-- | Insert a slice in the state (with replace)
insertSlice :: SliceID -> Slice -> NRMState -> NRMState
insertSlice sliceID slice s = s {slices = LM.insert sliceID slice (slices s)}

-- | NRM state map view by ProcessID.
pidMap :: NRMState -> LMap ProcessID (CmdID, Cmd, SliceID, Slice)
pidMap s = mconcat $ LM.toList (slices s) <&> mkMap
  where
    mkMap :: forall c. (c, Slice) -> LMap ProcessID (CmdID, Cmd, c, Slice)
    mkMap x@(_, c) =
      LM.fromList $
        zip (pid <$> LM.elems (cmds c))
          (LM.toList (cmds c) <&> mkTriple x)

mkTriple :: (c, d) -> (a, b) -> (a, b, c, d)
mkTriple (cid, c) (cmid, cm) = (cmid, cm, cid, c)

-- | NRM state map view by cmdID of "running" commands..
cmdIDMap :: NRMState -> LMap CmdID (Cmd, SliceID, Slice)
cmdIDMap = mkCmdIDMap cmds

-- | NRM state map view by cmdID of "awaiting" commands.
awaitingCmdIDMap :: NRMState -> LMap CmdID (CmdCore, SliceID, Slice)
awaitingCmdIDMap = mkCmdIDMap awaiting

mkCmdIDMap
  :: Ord k
  => (Slice -> LMap k a)
  -> NRMState
  -> LMap k (a, SliceID, Slice)
mkCmdIDMap accessor s = mconcat $ LM.toList (slices s) <&> mkMap
  where
    mkMap x@(_, c) =
      LM.fromList $
        zip (LM.keys $ accessor c)
          (LM.elems (accessor c) <&> mk x)
    mk :: (b, c) -> a -> (a, b, c)
    mk (cid, c) cm = (cm, cid, c)

{-# WARNING runningCmdIDCmdMap "To remove" #-}
-- | List commands currently registered as running
runningCmdIDCmdMap :: NRMState -> LMap CmdID Cmd
runningCmdIDCmdMap = cmdsMap cmds

-- | List commands awaiting to be launched
awaitingCmdIDCmdMap :: NRMState -> LMap CmdID CmdCore
awaitingCmdIDCmdMap = cmdsMap awaiting

-- | List commands awaiting to be launched
cmdsMap :: (Slice -> LMap CmdID a) -> NRMState -> LMap CmdID a
cmdsMap accessor s = mconcat $ accessor <$> LM.elems (slices s)
