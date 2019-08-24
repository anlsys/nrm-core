{-|
Module      : Nrm.NrmState
Copyright   : (c) 2019, UChicago Argonne, LLC.
License     : BSD3
Maintainer  : fre@freux.fr
-}
module Nrm.NrmState
  ( initialState
  , registerLibnrmDownstreamClient
  , registerFailed
  , registerLaunched
  , registerAwaiting
  , createContainer
  , removeContainer
  , listSensors
  , listActuators
  )
where

import qualified Data.Map as DM
import Nrm.Containers.Dummy as CD
import Nrm.Containers.Nodeos as CN
import Nrm.Containers.Singularity as CS
import Nrm.Node.Hwloc
import Nrm.Node.Sysfs
import Nrm.Node.Sysfs.Internal
import Nrm.Types.Actuator
import Nrm.Types.Configuration
import qualified Nrm.Types.Container as C
import Nrm.Types.DownstreamClient
import Nrm.Types.NrmState
import Nrm.Types.Process
import qualified Nrm.Types.Sensor as Sensor
import Nrm.Types.Topology
import Protolude

-- | Populate the initial NrmState.
initialState :: Cfg -> IO NrmState
initialState c = do
  hwl <- getHwlocData
  let packages' = DM.fromList $ (,Package {raplSensor = Nothing}) <$> selectPackageIDs hwl
  packages <-
    getDefaultRAPLDirs (toS $ raplPath $ raplCfg c) <&> \case
      Just (RAPLDirs rapldirs) -> Protolude.foldl goRAPL packages' rapldirs
      Nothing -> packages'
  return $ NrmState
    { containers = DM.fromList []
    , pus = DM.fromList $ (,PU) <$> selectPUIDs hwl
    , cores = DM.fromList $ (,Core) <$> selectCoreIDs hwl
    , dummyRuntime = if dummy c
    then Just CD.emptyRuntime
    else Nothing
    , singularityRuntime = if singularity c
    then Just SingularityRuntime
    else Nothing
    , nodeosRuntime = if nodeos c
    then Just NodeosRuntime
    else Nothing
    , ..
    }
  where
    goRAPL m RAPLDir {..} = DM.adjust (addRAPLSensor path maxEnergy) pkgid m
    addRAPLSensor path maxEnergy Package {..} = Package
      { raplSensor = Just
          ( Sensor.RaplSensor
            { raplPath = path
            , max = maxEnergy
            }
          )
      , ..
      }

-- | List sensors
listSensors :: NrmState -> [Sensor.Sensor]
listSensors = undefined

-- | List actuators
listActuators :: NrmState -> [Actuator]
listActuators = undefined

-- | TODO
registerLibnrmDownstreamClient :: NrmState -> DownstreamThreadID -> NrmState
registerLibnrmDownstreamClient s _ = s

-- | Removes a container from the state
removeContainer :: C.ContainerID -> NrmState -> NrmState
removeContainer containerID st =
  st {containers = DM.delete containerID (containers st)}

-- | Registers a container if not already tracked in the state, and returns the new state.
createContainer :: C.ContainerID -> NrmState -> NrmState
createContainer containerID st =
  case DM.lookup containerID (containers st) of
    Nothing -> st {containers = containers'}
      where
        containers' = DM.insert containerID C.emptyContainer (containers st)
    Just _ -> st

-- | Registers an awaiting command in an existing container
registerAwaiting :: CmdID -> Cmd -> C.ContainerID -> NrmState -> NrmState
registerAwaiting cmdID cmdValue containerID st =
  st {containers = DM.update f containerID (containers st)}
  where
    f c = Just $ c {C.awaiting = DM.insert cmdID cmdValue (C.awaiting c)}

{-{ C.awaiting = DM.delete cmdID (awaiting container)-}
{-, C.cmds = DM.insert cmdID c (cmds container)-}

-- | Turns an awaiting command to a launched one.
registerLaunched :: CmdID -> NrmState -> (NrmState, C.ContainerID)
registerLaunched cmdID st =
  case DM.lookup cmdID (awaitingCmdIDContainerIDMap st) of
    Nothing -> panic "internal nrm.so lookup error."
    Just containerID -> case DM.lookup containerID (containers st) of
      Nothing -> panic "container was deleted while command was registering"
      Just container -> case DM.lookup cmdID (C.awaiting container) of
        Nothing -> panic "internal nrm.so lookup error"
        Just cmdValue ->
          ( st
              { containers = DM.insert containerID
                  ( container
                    { C.cmds = DM.insert cmdID cmdValue (C.cmds container)
                    , C.awaiting = DM.delete cmdID (C.awaiting container)
                    }
                  )
                  (containers st)
              }
          , containerID
          )

-- | Fails an awaiting command.
registerFailed :: CmdID -> NrmState -> NrmState
registerFailed cmdID st =
  case DM.lookup cmdID (awaitingCmdIDContainerIDMap st) of
    Nothing ->
      panic $ "pynrm/nrm.so interaction error: " <>
        "command was not registered as awaiting in any container"
    Just containerID -> st {containers = DM.update f containerID (containers st)}
  where
    f c =
      if null (C.cmds c)
      then Nothing
      else Just $ c {C.awaiting = DM.delete cmdID (C.awaiting c)}
