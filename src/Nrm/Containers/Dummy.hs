{-# LANGUAGE NoImplicitPrelude #-}

{-|
Module      : Nrm.Containers.Dummy
Description : Dummy container runtime
Copyright   : (c) 2019, UChicago Argonne, LLC.
License     : BSD3
Maintainer  : fre@freux.fr

This module offers a dummy container runtime that doesn't create any container
but still launches commands.
-}
module Nrm.Containers.Dummy
  ( Dummy (..)
  )
where

import Nrm.Containers.Class
import Protolude

data Dummy = Dummy

instance ContainerRuntime Dummy where

  create = return Dummy

  execute = undefined

  stop = undefined
