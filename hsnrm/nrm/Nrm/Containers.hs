{-|
Module      : Nrm.Containers
Copyright   : (c) UChicago Argonne, 2019
License     : BSD3
Maintainer  : fre@freux.fr
-}
module Nrm.Containers
  ( ContainerRuntime (..)
  , MainRuntimeConfig (..)
  , TaggedRuntimes (..)
  , Runtimes (..)
  )
where

{-, getRuntime-}
import Nrm.Containers.Class
import Nrm.Containers.Dummy

{-import Nrm.Containers.Nodeos-}
{-import Nrm.Containers.Singularity-}
{-import Protolude-}
data MainRuntimeConfig
  = MainRuntimeConfig
      { dummy :: ()
      , dummy2 :: ()
      }

data TaggedRuntimes = TagDummy DummyRuntime | TagOther DummyRuntime

newtype Runtimes = Runtimes [TaggedRuntimes]

{-instance (MonadIO m) => ContainerRuntime m Runtime-}

{-getRuntime-}
{-:: (MonadIO m, ContainerRuntime m DummyRuntime ())-}
{-=> RuntimeName-}
{--> m (Either Text Runtime)-}
{-getRuntime NameDummy = (TagDummy <$>) <$> doEnableRuntime-}
{-getRuntime NameOther = (TagOther <$>) <$> doEnableRuntime-}
