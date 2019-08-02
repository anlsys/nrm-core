{-|
Module      : Nrm.Containers
Description : Containers interface
Copyright   : (c) UChicago Argonne, 2019
License     : BSD3
Maintainer  : fre@freux.fr
-}
module Nrm.Containers
  ( Nodeos (..)
  , Singularity (..)
  , Dummy (..)
  , ContainerRuntime (..)
  )
where

import Nrm.Containers.Class
import Nrm.Containers.Dummy
import Nrm.Containers.Nodeos
import Nrm.Containers.Singularity
