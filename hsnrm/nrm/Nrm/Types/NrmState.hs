{-|
Module      : Nrm.Types.NrmState
Copyright   : (c) UChicago Argonne, 2019
License     : BSD3
Maintainer  : fre@freux.fr
-}
module Nrm.Types.NrmState
  ( NrmState (..)
  , getCmds
  , lookupCmd
  , lookupContainer
  , updateContainer
  , adjustContainer
  , cmdIDMap
  , pidMap
  , runningCmdIDContainerIDMap
  , awaitingCmdIDContainerIDMap
  , runningCmdIDCmdMap
  , awaitingCmdIDCmdMap
  , -- * Rendering views
    showContainerList
  , showContainers
  )
where

import Data.Aeson
import Data.JSON.Schema
import Data.Map as DM
import Data.MessagePack
import Nrm.Containers.Dummy
import Nrm.Containers.Nodeos
import Nrm.Containers.Singularity
import Nrm.Types.Container as C
import Nrm.Types.Process as P
import Nrm.Types.Topology
import Protolude

data NrmState
  = NrmState
      { pus :: Map PUID PU
      , cores :: Map CoreID Core
      , packages :: Map PackageID Package
      , containers :: Map ContainerID Container
      , dummyRuntime :: Maybe DummyRuntime
      , singularityRuntime :: Maybe SingularityRuntime
      , nodeosRuntime :: Maybe NodeosRuntime
      }
  deriving (Show, Generic, MessagePack, ToJSON, FromJSON)

instance JSONSchema NrmState where

  schema _ = schema (Proxy :: Proxy Text)

showContainerList :: [(ContainerID, Container)] -> Text
showContainerList l =
  mconcat $ l <&> \(containerID, Container {..}) ->
    "container: ID " <> C.toText containerID <> "\n" <> mconcat (descCmd <$> DM.toList cmds)
  where
    descCmd (cmdID, cmdCore -> P.CmdCore {..}) =
      " command: ID " <> P.toText cmdID <> descSpec cmdPath arguments <> "\n"
    descSpec (P.Command cmd) (Arguments args) =
      " : " <> toS cmd <> " " <> (mconcat . intersperse " " $ toS <$> args)

-- | Renders a textual view of running containers
showContainers :: NrmState -> Text
showContainers NrmState {..} =
  showContainerList $ DM.toList containers

-- | Looks up a command via ID
lookupCmd :: CmdID -> NrmState -> Maybe Cmd
lookupCmd cmdID s = DM.lookup cmdID (mconcat $ cmds <$> DM.elems (containers s))

-- | Update a container, with optional deletion
updateContainer :: (Container -> Maybe Container) -> ContainerID -> NrmState -> NrmState
updateContainer f containerID s = s {containers = DM.update f containerID (containers s)}

-- | Adjust a container
adjustContainer :: (Container -> Container) -> ContainerID -> NrmState -> NrmState
adjustContainer f containerID s = s {containers = DM.adjust f containerID (containers s)}

-- | Looks up a command via ID
lookupContainer :: ContainerID -> NrmState -> Maybe Container
lookupContainer containerID s = DM.lookup containerID (containers s)

-- | Nrm state map view by ProcessID.
pidMap :: NrmState -> DM.Map ProcessID (CmdID, Cmd, ContainerID, Container)
pidMap s = mconcat $ DM.toList (containers s) <&> mkMap
  where
    mkMap x@(_, c) = DM.fromList $ zip (pid <$> DM.elems (cmds c)) (DM.toList (cmds c) <&> mkTriple x)
    mkTriple (cid, c) (cmid, cm) = (cmid, cm, cid, c)

-- | Nrm state map view by cmdID.
cmdIDMap :: NrmState -> DM.Map CmdID (Cmd, ContainerID, Container)
cmdIDMap s = mconcat $ DM.toList (containers s) <&> mkMap
  where
    mkMap x@(_, c) = DM.fromList $ zip (DM.keys $ cmds c) (DM.elems (cmds c) <&> mkTriple x)
    mkTriple (cid, c) cm = (cm, cid, c)

{-# WARNING runningCmdIDContainerIDMap "To remove" #-}
-- | Generate a map of all commands currently registered as running, and the associated containerID
runningCmdIDContainerIDMap :: NrmState -> DM.Map CmdID ContainerID
runningCmdIDContainerIDMap = containerMap cmds

-- | Generate a map of all commands currently registered as awaiting, and the associated containerID
awaitingCmdIDContainerIDMap :: NrmState -> DM.Map CmdID ContainerID
awaitingCmdIDContainerIDMap = containerMap awaiting

{-# WARNING runningCmdIDCmdMap "To remove" #-}
-- | List commands currently registered as running
runningCmdIDCmdMap :: NrmState -> DM.Map CmdID Cmd
runningCmdIDCmdMap = cmdsMap cmds

-- | List commands awaiting to be launched
awaitingCmdIDCmdMap :: NrmState -> DM.Map CmdID CmdCore
awaitingCmdIDCmdMap = cmdsMap awaiting

-- | Helper
containerMap :: (Container -> Map CmdID a) -> NrmState -> Map CmdID ContainerID
containerMap accessor s = mconcat $ f <$> DM.toList (containers s)
  where
    f :: (ContainerID, Container) -> Map CmdID ContainerID
    f (containerID, container) = fromList $ (,containerID) <$> DM.keys (accessor container)

-- | List commands awaiting to be launched
cmdsMap :: (Container -> Map CmdID a) -> NrmState -> DM.Map CmdID a
cmdsMap accessor s = mconcat $ accessor <$> elems (containers s)

-- | get all Cmds IDs for a container ID
getCmds :: NrmState -> C.ContainerID -> [CmdID]
getCmds st containerID = case DM.lookup containerID (containers st) of
  Nothing -> panic "containerID not found"
  Just c -> DM.keys $ C.cmds c
