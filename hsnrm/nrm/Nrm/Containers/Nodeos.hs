{-|
Module      : Nrm.Containers.Nodeos
Copyright   : (c) 2019, UChicago Argonne, LLC.
License     : BSD3
Maintainer  : fre@freux.fr

This module offers an interface to NodeOS containers.
-}
module Nrm.Containers.Nodeos
  ( NodeosRuntime (..)
  )
where

import Protolude
import Data.MessagePack

data NodeosRuntime = NodeosRuntime
  deriving (Show,Generic)

deriving instance MessagePack NodeosRuntime
