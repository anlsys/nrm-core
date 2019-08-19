{-|
Module      : Nrm.NrmState
Copyright   : (c) 2019, UChicago Argonne, LLC.
License     : BSD3
Maintainer  : fre@freux.fr
-}
module Nrm.NrmState
  ( initialState
  , registerLibnrmDownstreamClient
  , listSensors
  , listActuators
  )
where

import Data.Map
import Nrm.Containers.Dummy as CD
import Nrm.Containers.Nodeos as CN
import Nrm.Containers.Singularity as CS
import Nrm.Node.Hwloc
import Nrm.Types.Configuration
import Nrm.Types.DownstreamClient
import Nrm.Types.NrmState
import Nrm.Types.Topology
import Nrm.Types.Sensor
import Nrm.Types.Actuator
import Protolude

initialState :: Cfg -> IO NrmState
initialState c = do
  hwl <- getHwlocData
  return $ NrmState
    { containers = fromList []
    , pus = fromList []
    , cores = fromList []
    , packages = fromList []
    , downstreamCmds = fromList []
    , downstreamThreads = fromList []
    , topo = Topology
      { puIDs = selectPUIDs hwl
      , coreIDs = selectCoreIDs hwl
      , packageIDs = selectPackageIDs hwl
      }
    , dummyRuntime = if dummy c then Just CD.emptyRuntime else Nothing
    , singularityRuntime = if singularity c then Just SingularityRuntime else Nothing
    , nodeosRuntime = if nodeos c then Just NodeosRuntime else Nothing
    }

listSensors :: NrmState -> [Sensor]
listSensors = undefined

listActuators :: NrmState -> [Actuator]
listActuators = undefined

-- | TODO
registerLibnrmDownstreamClient :: NrmState -> DownstreamThreadID -> NrmState
registerLibnrmDownstreamClient s _ = s
