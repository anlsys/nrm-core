-- |
-- Copyright   : (c) 2019, UChicago Argonne, LL
-- License     : BSD3
-- Maintainer  : swann@anl.gov
module NRM.State
  ( -- * Initial state
    initialState,

    -- * Creation/Registration
    createSlice,
    registerAwaiting,
    registerFailed,
    registerLaunched,

    -- ** Command removal
    CmdKey (..),
    DeletionInfo (..),
    removeCmd,
  )
where

import CPD.Integrated as I
import Control.Lens
import Data.Map as M
import Data.Map.Merge.Lazy
import NRM.Node.Hwloc
import NRM.Node.Sysfs
import NRM.Node.Sysfs.Internal
import NRM.Types.Cmd
import NRM.Types.CmdID
import qualified NRM.Types.Configuration as Cfg
import NRM.Types.Controller
import NRM.Types.MemBuffer as MemBuffer
import NRM.Types.Process
import NRM.Types.Slice
import NRM.Types.State as NRMState
import NRM.Types.Topology
import NRM.Types.Topology.Package
import NRM.Types.Units
import NRM.Types.UpstreamClient
import Protolude

-- | Populate the initial NRMState.
initialState :: Cfg.Cfg -> Time -> IO NRMState
initialState c time = do
  hwl <- getHwlocData
  let packages' = M.fromList $ selectPackageIDs hwl <&> (,Package {rapl = Nothing})
  packages <-
    Cfg.raplCfg c & \case
      Nothing -> pure packages'
      Just raplc -> do
        defaultDirs <- getDefaultRAPLDirs (toS $ Cfg.raplPath raplc)
        defaultDirs & \case
          Nothing -> pure packages'
          Just (RAPLDirs rapldirs) -> do
            configs <- forM rapldirs (readRAPLConfiguration . path)
            let fullMap =
                  merge
                    dropMissing
                    dropMissing
                    (zipWithMaybeMatched (\_ mrapl rapldir -> (,rapldir) <$> mrapl))
                    configs
                    rapldirs
                updater _pkgid package (packageRaplConfig, packageRaplDir) =
                  package
                    { rapl = Just Rapl
                        { frequency = Cfg.toFrequency $ Cfg.passiveSensorFrequency c,
                          raplCfg = packageRaplConfig,
                          maxEnergyCounterValue = packageRaplDir ^. #maxEnergy,
                          max = watts 150,
                          defaultPower =
                            uW $
                              Cfg.microwatts (Cfg.referencePower raplc),
                          discreteChoices =
                            uW . Cfg.microwatts <$> Cfg.raplActions raplc,
                          lastRead = Nothing,
                          history = MemBuffer.empty
                        }
                    }
                newPkgs =
                  merge
                    preserveMissing
                    dropMissing
                    (zipWithMatched updater)
                    packages'
                    fullMap
            return newPkgs
  return NRMState
    { controller = Cfg.controlCfg c & \case
        Cfg.ControlOff -> Nothing
        ccfg ->
          Just $
            initialController
              IntegratorMeta
                { tLast = time,
                  I.minimumWaitInterval = Cfg.toTime $ Cfg.minimumWaitInterval ccfg,
                  I.minimumControlInterval = Cfg.toTime $ Cfg.minimumControlInterval ccfg
                }
              [],
      slices = M.fromList [],
      pus = M.fromList $ (,PU) <$> selectPUIDs hwl,
      cores = M.fromList $ (,Core) <$> selectCoreIDs hwl,
      extraStaticActuators =
        Cfg.extraStaticActuators c
          & fmap (\Cfg.ActuatorKV {actuatorID, actuator} -> (actuatorID, actuator))
          & M.fromList,
      extraStaticPassiveSensors =
        ( Cfg.extraStaticPassiveSensors c
            & fmap (\Cfg.SensorKV {sensorID, sensor} -> (sensorID, sensor))
            & M.fromList
        )
          <&> concretizeExtraPassiveSensor
            (Cfg.toFrequency $ Cfg.passiveSensorFrequency c),
      ..
    }

concretizeExtraPassiveSensor ::
  Frequency ->
  Cfg.ExtraPassiveSensor ->
  NRMState.ExtraPassiveSensor
concretizeExtraPassiveSensor f x =
  NRMState.ExtraPassiveSensor
    { NRMState.extraPassiveSensor = x,
      NRMState.history = [],
      NRMState.lastRead = Nothing,
      NRMState.frequency = f
    }

-- | Removes a slice from the state
removeSlice :: SliceID -> NRMState -> (Maybe Slice, NRMState)
removeSlice sliceID st =
  ( M.lookup sliceID (slices st),
    st {slices = M.delete sliceID (slices st)}
  )

-- | Result annotation for command removal from the state.
data DeletionInfo
  = -- | If the slice was removed as a result of the command deletion
    SliceRemoved
  | -- | If the command was removed but the slice stayed.
    CmdRemoved

-- | Wrapper for the type of key to lookup commands on
data CmdKey
  = KCmdID CmdID
  | KProcessID ProcessID

-- | Removes a command from the state, and also removes the slice if it's
-- empty as a result.
removeCmd ::
  CmdKey ->
  NRMState ->
  Maybe (DeletionInfo, CmdID, Cmd, SliceID, NRMState)
removeCmd key st = case key of
  KCmdID cmdID ->
    M.lookup cmdID (cmdIDMap st) <&> \(cmd, sliceID, slice) ->
      go cmdID cmd sliceID slice
  KProcessID pid ->
    M.lookup pid (pidMap st) <&> \(cmdID, cmd, sliceID, slice) ->
      go cmdID cmd sliceID slice
  where
    go cmdID cmd sliceID slice =
      if length (cmds slice) == 1
        then (SliceRemoved, cmdID, cmd, sliceID, snd $ removeSlice sliceID st)
        else
          ( CmdRemoved,
            cmdID,
            cmd,
            sliceID,
            st & #slices . ix sliceID . #cmds %~ sans cmdID
          )

-- | Registers a slice if not already tracked in the state, and returns the new state.
createSlice ::
  SliceID ->
  NRMState ->
  NRMState
createSlice sliceID st =
  st ^. #slices . at sliceID & \case
    Nothing -> st & #slices . at sliceID ?~ emptySlice
    Just _ -> st

-- | Registers an awaiting command in an existing slice
registerAwaiting :: CmdID -> CmdCore -> SliceID -> NRMState -> NRMState
registerAwaiting cmdID cmdValue sliceID =
  #slices . ix sliceID . #awaiting . at cmdID ?~ cmdValue

-- | Turns an awaiting command to a launched one.
registerLaunched ::
  CmdID ->
  ProcessID ->
  NRMState ->
  Either Text (NRMState, SliceID, Maybe UpstreamClientID)
registerLaunched cmdID pid st =
  case M.lookup cmdID (awaitingCmdIDMap st) of
    Nothing -> Left "No such awaiting command."
    Just (cmdCore, sliceID, slice) ->
      Right
        ( st & #slices . at sliceID
            ?~ ( slice &~ do
                   #cmds . at cmdID ?= registerPID cmdID cmdCore pid
                   #awaiting %= sans cmdID
               ),
          sliceID,
          upstreamClientID cmdCore
        )

-- | Fails an awaiting command.
registerFailed ::
  CmdID ->
  NRMState ->
  Maybe (NRMState, SliceID, Slice, CmdCore)
registerFailed cmdID st =
  awaitingCmdIDMap st ^. at cmdID <&> \(cmdCore, sliceID, slice) ->
    ( st & #slices . at sliceID %~ (>>= f),
      sliceID,
      slice,
      cmdCore
    )
  where
    f :: Slice -> Maybe Slice
    f c =
      if M.null (cmds c)
        then Nothing
        else Just $ c & #awaiting %~ sans cmdID
