{-|
Module      : Nrm.Types.Topology.PU
Copyright   : (c) UChicago Argonne, 2019
License     : BSD3
Maintainer  : fre@freux.fr
-}
module Nrm.Types.Topology.PU
  ( PU (..)
  )
where

import Data.Aeson
import Data.Either
import qualified Data.Map as DM
import Data.MessagePack
import Protolude
import Refined
import Refined.Orphan.Aeson ()
import Prelude (String, fail)
-- | Record containing all information about a processing unit.
data PU = PU
  deriving (Show, Generic, MessagePack, ToJSON, FromJSON)
