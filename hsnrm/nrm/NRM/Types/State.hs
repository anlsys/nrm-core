-- |
-- Module      : NRM.Types.State
-- Copyright   : (c) UChicago Argonne, 2019
-- License     : BSD3
-- Maintainer  : fre@freux.fr
module NRM.Types.State
  ( NRMState (..),

    -- * Insertion
    insertSlice,

    -- * Useful maps
    cmdIDMap,
    pidMap,
    awaitingCmdIDMap,

    -- * lookups
    lookupProcess,

    -- * Rendering views
    showSliceList,
    showSlices,

    -- * Lenses
    _cmdID,
    _sliceID,
  )
where

import Control.Lens
import Data.Aeson
import Data.Data
import Data.Generics.Labels ()
import Data.JSON.Schema
import Data.MessagePack
import LMap.Map as LM
import LensMap.Core
import NRM.Slices.Dummy
import NRM.Slices.Nodeos
import NRM.Slices.Singularity
import NRM.Types.Actuator
import NRM.Types.Cmd
import NRM.Types.CmdID as CmdID
import NRM.Types.Controller
import NRM.Types.Process as P
import NRM.Types.Sensor
import NRM.Types.Slice as C
import NRM.Types.Topology
import Protolude

data NRMState
  = NRMState
      { pus :: LM.Map PUID PU,
        cores :: LM.Map CoreID Core,
        packages :: LM.Map PackageID Package,
        slices :: LM.Map SliceID Slice,
        dummyRuntime :: Maybe DummyRuntime,
        singularityRuntime :: Maybe SingularityRuntime,
        nodeosRuntime :: Maybe NodeosRuntime,
        controller :: Maybe Controller
      }
  deriving (Show, Generic, MessagePack, ToJSON, FromJSON)

instance HasLensMap NRMState ActuatorKey Actuator where
  lenses s =
    mconcat
      [ addPath #packages <$> lenses (view #packages s)
      ]

instance HasLensMap NRMState ActiveSensorKey ActiveSensor where
  lenses s =
    mconcat
      [ addPath #slices <$> lenses (view #slices s)
      ]

instance HasLensMap NRMState PassiveSensorKey PassiveSensor where
  lenses s =
    mconcat
      [ addPath #packages <$> lenses (view #packages s)
      ]

instance JSONSchema NRMState where
  schema _ = schema (Proxy :: Proxy Text)

showSliceList :: [(SliceID, Slice)] -> Text
showSliceList l =
  mconcat $
    l <&> \(sliceID, Slice {..}) ->
      "slice: ID " <> C.toText sliceID <> "\n" <> mconcat (descCmd <$> LM.toList cmds)
  where
    descCmd (cmdID, cmdCore -> CmdCore {..}) =
      " command: ID " <> CmdID.toText cmdID
        <> descSpec cmdPath arguments
        <> "\n"
    descSpec (Command cmd) args =
      " : " <> toS cmd <> " " <> (mconcat . intersperse " " $ showArg <$> args)
    showArg (Arg a) = a

-- | Renders a textual view of running slices
showSlices :: NRMState -> Text
showSlices NRMState {..} =
  showSliceList $ LM.toList slices

-- | Insert a slice in the state (with replace)
insertSlice :: SliceID -> Slice -> NRMState -> NRMState
insertSlice sliceID slice s = s {slices = LM.insert sliceID slice (slices s)}

lookupProcess :: ProcessID -> NRMState -> Maybe (CmdID, Cmd, SliceID, Slice)
lookupProcess cmdID st = LM.lookup cmdID (pidMap st)

-- | NRM state map view by ProcessID.
pidMap :: NRMState -> LM.Map ProcessID (CmdID, Cmd, SliceID, Slice)
pidMap s = mconcat $ LM.toList (slices s) <&> mkMap
  where
    mkMap :: forall c. (c, Slice) -> LM.Map ProcessID (CmdID, Cmd, c, Slice)
    mkMap x@(_, c) =
      LM.fromList $
        zip
          (pid <$> LM.elems (cmds c))
          (LM.toList (cmds c) <&> mkTriple x)

mkTriple :: (c, d) -> (a, b) -> (a, b, c, d)
mkTriple (cid, c) (cmid, cm) = (cmid, cm, cid, c)

lookupCmd :: CmdID -> NRMState -> Maybe (Cmd, SliceID, Slice)
lookupCmd cmdID st = LM.lookup cmdID (cmdIDMap st)

-- | NRM state map view by cmdID of "running" commands.
cmdIDMap :: NRMState -> LM.Map CmdID (Cmd, SliceID, Slice)
cmdIDMap = mkCmdIDMap cmds

-- | NRM state map view by cmdID of "awaiting" commands.
awaitingCmdIDMap :: NRMState -> LM.Map CmdID (CmdCore, SliceID, Slice)
awaitingCmdIDMap = mkCmdIDMap awaiting

mkCmdIDMap ::
  Ord k =>
  (Slice -> LM.Map k a) ->
  NRMState ->
  LM.Map k (a, SliceID, Slice)
mkCmdIDMap accessor s = mconcat $ LM.toList (slices s) <&> mkMap
  where
    mkMap x@(_, c) =
      LM.fromList $
        zip
          (LM.keys $ accessor c)
          (LM.elems (accessor c) <&> mk x)
    mk :: (b, c) -> a -> (a, b, c)
    mk (cid, c) cm = (cm, cid, c)

-- Lenses
_sliceID :: SliceID -> Lens' NRMState (Maybe Slice)
_sliceID sliceID = #slices . at sliceID

_cmdID :: CmdID -> Lens' NRMState (Maybe Cmd)
_cmdID cmdID = lens getter setter
  where
    getter :: NRMState -> Maybe Cmd
    getter st = lookupCmd cmdID st <&> \(cmd, _, _) -> cmd
    setter st (Just cmd) =
      lookupCmd cmdID st & \case
        Just (_, sliceID, slice) ->
          st & _sliceID sliceID ?~ (slice & (#cmds . at cmdID) ?~ cmd)
        Nothing -> st
    setter st Nothing =
      lookupCmd cmdID st & \case
        Just (_, sliceID, _) ->
          st & _sliceID sliceID %~ mayRemoveSlice
        Nothing -> st
    mayRemoveSlice :: Maybe Slice -> Maybe Slice
    mayRemoveSlice x =
      x >>= \slice ->
        if length (cmds slice) == 1
          then Nothing
          else Just $ slice & #cmds . at cmdID .~ Nothing
