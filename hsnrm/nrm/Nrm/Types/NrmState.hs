{-|
Module      : Nrm.Types.NrmState
Copyright   : (c) UChicago Argonne, 2019
License     : BSD3
Maintainer  : fre@freux.fr
-}
module Nrm.Types.NrmState
  ( NrmState (..)
  , initialState
  )
where

import Data.MessagePack
import Nrm.Types.Configuration
import Nrm.Types.Topology
import Protolude

data NrmState
  = NrmState
      { topo :: Topology
      }
  deriving (Generic)

initialState :: Cfg -> IO NrmState
initialState _ = return $ NrmState {topo = Topology}

deriving instance MessagePack NrmState
