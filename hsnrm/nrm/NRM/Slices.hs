{-|
Module      : NRM.Slices
Copyright   : (c) UChicago Argonne, 2019
License     : BSD3
Maintainer  : fre@freux.fr
-}
module NRM.Slices
  (
  {-Slice (..)-}
  {-, Runtime (..)-}
  )
where

{-, getRuntime-}
{-import NRM.Slices.Class-}
{-import NRM.Slices.Dummy-}
{-import Protolude-}
{-import Data.MessagePack-}

{-data Runtime = DummyRuntime | NodeOSRuntime | SingularityRuntime-}
  {-deriving (Show, Generic, MessagePack)-}

{-data Slice = Slice {runtime :: Runtime}-}
  {-deriving (Show, Generic, MessagePack)-}

{-import NRM.Slices.Nodeos-}
{-import NRM.Slices.Singularity-}
{-import Protolude-}
{-data MainRuntimeConfig-}
{-= MainRuntimeConfig-}
{-{ dummy :: ()-}
{-, dummy2 :: ()-}
{-}-}

{-data TaggedRuntimes = TagDummy DummyRuntime | TagOther DummyRuntime-}

{-newtype Runtimes = Runtimes [TaggedRuntimes]-}

{-instance (MonadIO m) => SliceRuntime m Runtime-}

{-getRuntime-}
{-:: (MonadIO m, SliceRuntime m DummyRuntime ())-}
{-=> RuntimeName-}
{--> m (Either Text Runtime)-}
{-getRuntime NameDummy = (TagDummy <$>) <$> doEnableRuntime-}
{-getRuntime NameOther = (TagOther <$>) <$> doEnableRuntime-}
