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

data Dummy = Dummy

{-class DummyRuntime(ContainerRuntime):-}

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
