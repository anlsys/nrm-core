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
    descCmd (cmdID, P.Cmd {..}) =
      " command: ID " <> P.toText cmdID <> descSpec spec <> "\n"
    descSpec (P.CmdSpec cmd (Arguments args) _) =
      " : " <> toS cmd <> " " <> (mconcat . intersperse " " $ toS <$> args)

-- | Renders a textual view of running containers
showContainers :: NrmState -> Text
showContainers NrmState {..} =
  showContainerList $ DM.toList containers

-- | Looks up a command via ID
lookupCmd :: CmdID -> NrmState -> Maybe Cmd
lookupCmd cmdID s = DM.lookup cmdID (mconcat $ cmds <$> DM.elems (containers s))

-- | Looks up a command via ID
{-updateContainer :: ContainerID -> NrmState -> Maybe Container-}
updateContainer f containerID s = s {containers = DM.update f containerID (containers s)}
adjustContainer f containerID s = s {containers = DM.adjust f containerID (containers s)}

-- | Looks up a command via ID
lookupContainer :: ContainerID -> NrmState -> Maybe Container
lookupContainer containerID s = DM.lookup containerID (containers s)

-- | Generate a map of all commands currently registered as running, and the associated containerID
runningCmdIDContainerIDMap :: NrmState -> DM.Map CmdID ContainerID
runningCmdIDContainerIDMap = containerMap cmds

-- | Generate a map of all commands currently registered as awaiting, and the associated containerID
awaitingCmdIDContainerIDMap :: NrmState -> DM.Map CmdID ContainerID
awaitingCmdIDContainerIDMap = containerMap awaiting

-- | List commands currently registered as running
runningCmdIDCmdMap :: NrmState -> DM.Map CmdID Cmd
runningCmdIDCmdMap = cmdsMap cmds

-- | List commands awaiting to be launched
awaitingCmdIDCmdMap :: NrmState -> DM.Map CmdID Cmd
awaitingCmdIDCmdMap = cmdsMap awaiting

-- | Helper
containerMap :: (Container -> Map CmdID a) -> NrmState -> Map CmdID ContainerID
containerMap accessor s = mconcat $ f <$> DM.toList (containers s)
  where
    f :: (ContainerID, Container) -> Map CmdID ContainerID
    f (containerID, container) = fromList $ (,containerID) <$> DM.keys (accessor container)

-- | List commands awaiting to be launched
cmdsMap :: (Container -> Map CmdID Cmd) -> NrmState -> DM.Map CmdID Cmd
cmdsMap accessor s = mconcat $ accessor <$> elems (containers s)

-- | get all Cmds IDs for a container ID
getCmds :: NrmState -> C.ContainerID -> [CmdID]
getCmds st containerID = case DM.lookup containerID (containers st) of
  Nothing -> panic "containerID not found"
  Just c -> DM.keys $ C.cmds c
