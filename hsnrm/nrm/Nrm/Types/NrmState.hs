{-|
Module      : Nrm.Types.NrmState
Copyright   : (c) UChicago Argonne, 2019
License     : BSD3
Maintainer  : fre@freux.fr
-}
module Nrm.Types.NrmState
  ( NrmState (..)
  )
where

import Data.MessagePack
import Nrm.Containers
import Nrm.Containers.Dummy
import Nrm.Containers.Nodeos
import Nrm.Containers.Singularity
import Nrm.Types.Container
import Nrm.Types.DownstreamClient
import Nrm.Types.Topology
import Protolude

data NrmState
  = NrmState
      { topo :: Topology
      , pus :: Map PUID PU
      , cores :: Map CoreID Core
      , packages :: Map PackageID Package
      , containers :: Map ContainerID Container
      , downstreamCmds :: Map DownstreamCmdID DownstreamCmd
      , downstreamThreads :: Map DownstreamThreadID DownstreamThread
      , dummyRuntime :: Maybe DummyRuntime
      , singularityRuntime :: Maybe SingularityRuntime
      , nodeosRuntime :: Maybe NodeosRuntime
      }
  deriving (Show, Generic, MessagePack)
