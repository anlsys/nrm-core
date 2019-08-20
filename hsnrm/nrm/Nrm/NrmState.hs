{-|
Module      : Nrm.NrmState
Copyright   : (c) 2019, UChicago Argonne, LLC.
License     : BSD3
Maintainer  : fre@freux.fr
-}
module Nrm.NrmState
  ( initialState
  , registerLibnrmDownstreamClient
  , -- * Views for containers and commands
    runningCmdIDContainerIDMap
  , awaitingCmdIDContainerIDMap
  , runningCmdIDCmdMap
  , awaitingCmdIDCmdMap
  , -- * Views for controllers
    listSensors
  , listActuators
  )
where

import Data.Map as DM
import Nrm.Containers.Dummy as CD
import Nrm.Containers.Nodeos as CN
import Nrm.Containers.Singularity as CS
import Nrm.Node.Hwloc
import Nrm.Types.Actuator
import Nrm.Types.Configuration
import Nrm.Types.Container
import Nrm.Types.DownstreamClient
import Nrm.Types.NrmState
import Nrm.Types.Process
import qualified Nrm.Types.Sensor as Sensor
import Nrm.Types.Topology
import Protolude

initialState :: Cfg -> IO NrmState
initialState c = do
  hwl <- getHwlocData
  return $ NrmState
    { containers = fromList []
    , topo = Topology
      { puIDs = DM.fromList $ (,PU) <$> selectPUIDs hwl
      , coreIDs = DM.fromList $ (,Core) <$> selectCoreIDs hwl
      , packageIDs = DM.fromList $ (,Package) <$> selectPackageIDs hwl
      }
    , dummyRuntime = if dummy c
    then Just CD.emptyRuntime
    else Nothing
    , singularityRuntime = if singularity c
    then Just SingularityRuntime
    else Nothing
    , nodeosRuntime = if nodeos c
    then Just NodeosRuntime
    else Nothing
    }

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

-- | List sensors
listSensors :: NrmState -> [Sensor.Sensor]
listSensors = undefined

-- | List actuators
listActuators :: NrmState -> [Actuator]
listActuators = undefined

-- | TODO
registerLibnrmDownstreamClient :: NrmState -> DownstreamThreadID -> NrmState
registerLibnrmDownstreamClient s _ = s

-- | Helper
containerMap :: (Container -> Map CmdID a) -> NrmState -> Map CmdID ContainerID
containerMap accessor s = mconcat $ f <$> DM.toList (containers s)
  where
    f :: (ContainerID, Container) -> Map CmdID ContainerID
    f (containerID, container) = fromList $ (,containerID) <$> DM.keys (accessor container)

-- | List commands awaiting to be launched
cmdsMap :: (Container -> Map CmdID Cmd) -> NrmState -> DM.Map CmdID Cmd
cmdsMap accessor s = mconcat $ accessor <$> elems (containers s)
