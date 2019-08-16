{-|
Module      : Nrm.NrmState
Copyright   : (c) 2019, UChicago Argonne, LLC.
License     : BSD3
Maintainer  : fre@freux.fr
-}
module Nrm.NrmState
  ( initialState
  , registerLibnrmDownstreamClient
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
import Protolude

initialState :: Cfg -> IO NrmState
initialState c = do
  hwl <- getHwlocData
  return $ NrmState
    { containers = fromList []
    , topo = Topology
      { puIDs = selectPUIDs hwl
      , coreIDs = selectCoreIDs hwl
      , packageIDs = selectPackageIDs hwl
      }
    , dummyRuntime = if dummy c then Just CD.emptyRuntime else Nothing
    , singularityRuntime = if singularity c then Just SingularityRuntime else Nothing
    , nodeosRuntime = if nodeos c then Just NodeosRuntime else Nothing
    }

-- | TODO
registerLibnrmDownstreamClient :: NrmState -> DownstreamLibnrmID -> NrmState
registerLibnrmDownstreamClient s _ = s
