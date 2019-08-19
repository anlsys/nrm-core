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

import Data.MessagePack
import Protolude

data NodeosRuntime = NodeosRuntime
  deriving (Show, Generic, MessagePack)
