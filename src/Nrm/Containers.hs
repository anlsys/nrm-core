{-|
Module      : Nrm.Containers
Description : Containers interface
Copyright   : (c) UChicago Argonne, 2019
License     : BSD3
Maintainer  : fre@freux.fr
-}
module Nrm.Containers
  ( module Nrm.Containers.Nodeos
  , module Nrm.Containers.Singularity
  , module Nrm.Containers.Dummy
  , ContainerRuntime(..)
  )
where

import Nrm.Containers.Dummy
import Nrm.Containers.Nodeos
import Nrm.Containers.Singularity
import Protolude

class ContainerRuntime a where

  create :: IO a

  execute :: a -> IO a

  stop :: a -> IO ()

{-"""Implements a dummy runtime that doesn't create any container, but still-}
{-launches commands."""-}

{-def __init__(self):-}
{-pass-}

{-def create(self, container, downstream_uri):-}
{-pass-}

{-def execute(self, container_uuid, args, environ):-}
{-import tornado.process as process # type: ignore-}
{-return process.Subprocess(args,-}
{-stdout=process.Subprocess.STREAM,-}
{-stderr=process.Subprocess.STREAM,-}
{-env=environ)-}

{-def delete(self, container_uuid, kill=False):-}
{-pass-}
