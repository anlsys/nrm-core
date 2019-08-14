{-|
Module      : Nrm.NrmState
Copyright   : (c) 2019, UChicago Argonne, LLC.
License     : BSD3
Maintainer  : fre@freux.fr
-}
module Nrm.NrmState
  ( initialState
  , registerApplication
  )
where

import Nrm.Node.Hwloc
import Nrm.Types.Application
import Nrm.Types.Configuration
import Nrm.Types.Container
import Nrm.Containers.Dummy as CD
import Nrm.Containers.Singularity as CS
import Nrm.Containers.Nodeos as CN
import Nrm.Types.NrmState
import Nrm.Types.Topology
import Protolude

initialState :: Cfg -> IO NrmState
initialState c = do
  hwl <- getHwlocData
  return $ NrmState
    { topo = Topology
        { puIDs = selectPUIDs hwl
        , coreIDs = selectCoreIDs hwl
        , packageIDs = selectPackageIDs hwl
        }
    , dummyRuntime = if dummy c then Just CD.emptyRuntime else Nothing
    , singularityRuntime = if singularity c then Just SingularityRuntime else Nothing
    , nodeosRuntime = if nodeos c then Just NodeosRuntime else Nothing

    }

-- | TODO
registerApplication :: NrmState -> ContainerUUID -> ApplicationUUID -> NrmState
registerApplication s _ _ = s
