{-# LANGUAGE AllowAmbiguousTypes #-}
{-# LANGUAGE NoImplicitPrelude #-}

{-|
Module      : Nrm.Containers.Dummy
Description : Dummy container runtime
Copyright   : (c) 2019, UChicago Argonne, LLC.
License     : BSD3
Maintainer  : fre@freux.fr

-}
module Nrm.Containers.Class
  ( ContainerRuntime (..)
  )
where

import Nrm.Types.Applications
import Nrm.Types.Containers
import Protolude

class ContainerRuntime a streamInformation prepared where

  -- | list runtime returns a list of applications running in the container.
  lest :: a -> [ApplicationUUID]

  -- | initial state of the container runtime
  init :: a

  -- | creates a container using this container runtime.
  create :: IO (a, ContainerUUID)

  -- | prepares application for start in a container runtime. This should be
  -- immediately followed by `exec prepared` in libraries that link this code.
  execute :: a -> ContainerUUID -> StartData -> a

  -- | @exec x@ executes the application x using an exec system call.
  exec :: prepared -> IO ()

  -- | execute runtime containerUUID startData executes an application inside
  -- the container runtime. It returns the container as well as stream location
  -- information.
  exec :: a -> ContainerUUID -> StartData -> IO (a, streamInformation)

  stop :: a -> IO ()
