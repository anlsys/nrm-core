{-|
Module      : Nrm.Types.NrmState
Copyright   : (c) UChicago Argonne, 2019
License     : BSD3
Maintainer  : fre@freux.fr
-}
module Nrm.Types.NrmState
  ( NrmState (..)
  , -- * Insertion
    insertContainer
  , -- * Useful maps
    cmdIDMap
  , pidMap
  , awaitingCmdIDMap
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

-- | Insert a container in the state (with replace)
insertContainer :: ContainerID -> Container -> NrmState -> NrmState
insertContainer containerID container s = s {containers = DM.insert containerID container (containers s)}

-- | Nrm state map view by ProcessID.
pidMap :: NrmState -> DM.Map ProcessID (CmdID, Cmd, ContainerID, Container)
pidMap s = mconcat $ DM.toList (containers s) <&> mkMap
  where
    mkMap x@(_, c) =
      DM.fromList $
        zip (pid <$> DM.elems (cmds c))
          (DM.toList (cmds c) <&> mkTriple x)
    mkTriple (cid, c) (cmid, cm) = (cmid, cm, cid, c)

-- | Nrm state map view by cmdID of "running" commands..
cmdIDMap :: NrmState -> DM.Map CmdID (Cmd, ContainerID, Container)
cmdIDMap = mkCmdIDMap cmds

-- | Nrm state map view by cmdID of "awaiting" commands.
awaitingCmdIDMap :: NrmState -> DM.Map CmdID (CmdCore, ContainerID, Container)
awaitingCmdIDMap = mkCmdIDMap awaiting

mkCmdIDMap
  :: Ord k
  => (Container -> Map k a)
  -> NrmState
  -> Map k (a, ContainerID, Container)
mkCmdIDMap accessor s = mconcat $ DM.toList (containers s) <&> mkMap
  where
    mkMap x@(_, c) =
      DM.fromList $
        zip (DM.keys $ accessor c)
          (DM.elems (accessor c) <&> mkTriple x)
    mkTriple (cid, c) cm = (cm, cid, c)

{-# WARNING runningCmdIDCmdMap "To remove" #-}
-- | List commands currently registered as running
runningCmdIDCmdMap :: NrmState -> DM.Map CmdID Cmd
runningCmdIDCmdMap = cmdsMap cmds

-- | List commands awaiting to be launched
awaitingCmdIDCmdMap :: NrmState -> DM.Map CmdID CmdCore
awaitingCmdIDCmdMap = cmdsMap awaiting

-- | List commands awaiting to be launched
cmdsMap :: (Container -> Map CmdID a) -> NrmState -> DM.Map CmdID a
cmdsMap accessor s = mconcat $ accessor <$> elems (containers s)
