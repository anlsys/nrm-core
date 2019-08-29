{-|
Module      : Nrm.NrmState
Copyright   : (c) 2019, UChicago Argonne, LL
License     : BSD3
Maintainer  : fre@freux.fr
-}
module Nrm.NrmState
  ( -- * Initial state
    initialState
  , -- * Creation/Registration
    createContainer
  , registerLibnrmDownstreamClient
  , registerAwaiting
  , registerFailed
  , registerLaunched
  , -- * Removal
    -- ** Container removal
    removeContainer
  , -- ** Command removal
    CmdKey (..)
  , DeletionInfo (..)
  , removeCmd
  , -- * Queries
    listSensors
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
import Nrm.Types.Container
import Nrm.Types.DownstreamClient
import Nrm.Types.NrmState
import Nrm.Types.Process
import qualified Nrm.Types.Sensor as Sensor
import Nrm.Types.Topology
import Nrm.Types.UpstreamClient
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
removeContainer :: ContainerID -> NrmState -> (Maybe Container, NrmState)
removeContainer containerID st =
  ( DM.lookup containerID (containers st)
  , st {containers = DM.delete containerID (containers st)}
  )

-- | Result annotation for command removal from the state.
data DeletionInfo
  = -- | If the container was removed as a result of the command deletion
    ContainerRemoved
  | -- | If the command was removed but the container stayed.
    CmdRemoved

-- | Wrapper for the type of key to lookup commands on
data CmdKey = KCmdID CmdID | KProcessID ProcessID

-- | Removes a command from the state, and also removes the container if it's
-- empty as a result.
removeCmd
  :: CmdKey
  -> NrmState
  -> Maybe (DeletionInfo, CmdID, Cmd, ContainerID, NrmState)
removeCmd key st = case key of
  KCmdID cmdID ->
    DM.lookup cmdID (cmdIDMap st) <&> \(cmd, containerID, container) ->
      go cmdID cmd containerID container
  KProcessID pid ->
    DM.lookup pid (pidMap st) <&> \(cmdID, cmd, containerID, container) ->
      go cmdID cmd containerID container
  where
    go cmdID cmd containerID container =
      if length (cmds container) == 1
      then (ContainerRemoved, cmdID, cmd, containerID, snd $ removeContainer containerID st)
      else
        ( CmdRemoved
        , cmdID
        , cmd
        , containerID
        , insertContainer containerID
          (container {cmds = DM.delete cmdID (cmds container)})
          st
        )

-- | Registers a container if not already tracked in the state, and returns the new state.
createContainer
  :: ContainerID
  -> NrmState
  -> NrmState
createContainer containerID st =
  case DM.lookup containerID (containers st) of
    Nothing -> st {containers = containers'}
      where
        containers' = DM.insert containerID emptyContainer (containers st)
    Just _ -> st

-- | Registers an awaiting command in an existing container
registerAwaiting
  :: CmdID
  -> CmdCore
  -> ContainerID
  -> NrmState
  -> NrmState
registerAwaiting cmdID cmdValue containerID st =
  st {containers = DM.update f containerID (containers st)}
  where
    f c = Just $ c {awaiting = DM.insert cmdID cmdValue (awaiting c)}

{-{ awaiting = DM.delete cmdID (awaiting container)-}
{-, cmds = DM.insert cmdID c (cmds container)-}

-- | Turns an awaiting command to a launched one.
registerLaunched
  :: CmdID
  -> ProcessID
  -> NrmState
  -> Either Text (NrmState, ContainerID, Maybe UpstreamClientID)
registerLaunched cmdID pid st =
  case DM.lookup cmdID (awaitingCmdIDMap st) of
    Nothing -> Left "No such awaiting command."
    Just (cmdCore, containerID, container) ->
      Right
        ( st
            { containers = DM.insert containerID
                ( container
                  { cmds = DM.insert cmdID (registerPID cmdCore pid) (cmds container)
                  , awaiting = DM.delete cmdID (awaiting container)
                  }
                )
                (containers st)
            }
        , containerID
        , upstreamClientID cmdCore
        )

-- | Fails an awaiting command.
registerFailed
  :: CmdID
  -> NrmState
  -> Maybe (NrmState, ContainerID, Container, CmdCore)
registerFailed cmdID st =
  DM.lookup cmdID (awaitingCmdIDMap st) <&> \(cmdCore, containerID, container) ->
    (st {containers = DM.update f containerID (containers st)}, containerID, container, cmdCore)
  where
    f c =
      if null (cmds c)
      then Nothing
      else Just $ c {awaiting = DM.delete cmdID (awaiting c)}
