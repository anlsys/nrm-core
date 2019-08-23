{-|
Module      : Nrm.Types.NrmState
Copyright   : (c) UChicago Argonne, 2019
License     : BSD3
Maintainer  : fre@freux.fr
-}
module Nrm.Types.NrmState
  ( NrmState (..)
  , showContainerList
  , showContainers
  , lookupCmd
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

showContainers :: NrmState -> Text
showContainers NrmState {..} =
  showContainerList $ DM.toList containers

lookupCmd :: CmdID -> NrmState -> Maybe Cmd
lookupCmd cmdID s = DM.lookup cmdID (mconcat $ cmds <$> (DM.elems $ containers s))
