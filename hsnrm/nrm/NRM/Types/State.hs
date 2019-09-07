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

import Data.Aeson
import Data.JSON.Schema
import Data.Map as DM
import Data.MessagePack
import NRM.Slices.Dummy
import NRM.Slices.Nodeos
import NRM.Slices.Singularity
import NRM.Types.Cmd as Cmd
import NRM.Types.Process as P
import NRM.Types.Slice as C
import NRM.Types.Topology
import CPD.Core as CPD
import Protolude

data NRMState
  = NRMState
      { cpd :: CPD.Problem
      , pus :: Map PUID PU
      , cores :: Map CoreID Core
      , packages :: Map PackageID Package
      , slices :: Map SliceID Slice
      , dummyRuntime :: Maybe DummyRuntime
      , singularityRuntime :: Maybe SingularityRuntime
      , nodeosRuntime :: Maybe NodeosRuntime
      }
  deriving (Show, Generic, MessagePack, ToJSON, FromJSON)

instance JSONSchema NRMState where

  schema _ = schema (Proxy :: Proxy Text)


showSliceList :: [(SliceID, Slice)] -> Text
showSliceList l =
  mconcat $ l <&> \(sliceID, Slice {..}) ->
    "slice: ID " <> C.toText sliceID <> "\n" <> mconcat (descCmd <$> DM.toList cmds)
  where
    descCmd (cmdID, cmdCore -> CmdCore {..}) =
      " command: ID " <> Cmd.toText cmdID <> descSpec cmdPath arguments <> "\n"
    descSpec (Command cmd) (Arguments args) =
      " : " <> toS cmd <> " " <> (mconcat . intersperse " " $ toS <$> args)

-- | Renders a textual view of running slices
showSlices :: NRMState -> Text
showSlices NRMState {..} =
  showSliceList $ DM.toList slices

-- | Insert a slice in the state (with replace)
insertSlice :: SliceID -> Slice -> NRMState -> NRMState
insertSlice sliceID slice s = s {slices = DM.insert sliceID slice (slices s)}

-- | NRM state map view by ProcessID.
pidMap :: NRMState -> DM.Map ProcessID (CmdID, Cmd, SliceID, Slice)
pidMap s = mconcat $ DM.toList (slices s) <&> mkMap
  where
    mkMap x@(_, c) =
      DM.fromList $
        zip (pid <$> DM.elems (cmds c))
          (DM.toList (cmds c) <&> mkTriple x)
    mkTriple (cid, c) (cmid, cm) = (cmid, cm, cid, c)

-- | NRM state map view by cmdID of "running" commands..
cmdIDMap :: NRMState -> DM.Map CmdID (Cmd, SliceID, Slice)
cmdIDMap = mkCmdIDMap cmds

-- | NRM state map view by cmdID of "awaiting" commands.
awaitingCmdIDMap :: NRMState -> DM.Map CmdID (CmdCore, SliceID, Slice)
awaitingCmdIDMap = mkCmdIDMap awaiting

mkCmdIDMap
  :: Ord k
  => (Slice -> Map k a)
  -> NRMState
  -> Map k (a, SliceID, Slice)
mkCmdIDMap accessor s = mconcat $ DM.toList (slices s) <&> mkMap
  where
    mkMap x@(_, c) =
      DM.fromList $
        zip (DM.keys $ accessor c)
          (DM.elems (accessor c) <&> mkTriple x)
    mkTriple (cid, c) cm = (cm, cid, c)

{-# WARNING runningCmdIDCmdMap "To remove" #-}
-- | List commands currently registered as running
runningCmdIDCmdMap :: NRMState -> DM.Map CmdID Cmd
runningCmdIDCmdMap = cmdsMap cmds

-- | List commands awaiting to be launched
awaitingCmdIDCmdMap :: NRMState -> DM.Map CmdID CmdCore
awaitingCmdIDCmdMap = cmdsMap awaiting

-- | List commands awaiting to be launched
cmdsMap :: (Slice -> Map CmdID a) -> NRMState -> DM.Map CmdID a
cmdsMap accessor s = mconcat $ accessor <$> elems (slices s)
