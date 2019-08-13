{-|
Module      : Nrm.Types.NrmState
Copyright   : (c) UChicago Argonne, 2019
License     : BSD3
Maintainer  : fre@freux.fr
-}
module Nrm.Types.NrmState
  ( NrmState (..)
  , initialState
  , registerApplication
  )
where

import Data.MessagePack
import Nrm.Types.Application
import Nrm.Types.Configuration
import Nrm.Types.Container
import Nrm.Types.Topology
import Protolude

data NrmState
  = NrmState
      { topo :: Topology
      }
  deriving (Generic)

deriving instance MessagePack NrmState

initialState :: Cfg -> IO NrmState
initialState _ = return $ NrmState {topo = Topology}

registerApplication :: NrmState -> ContainerUUID -> ApplicationUUID -> NrmState
registerApplication s _ _ = s
