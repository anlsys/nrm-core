{-|
Module      : Nrm.Types.Container
Description : Container
Copyright   : (c) UChicago Argonne, 2019
License     : BSD3
Maintainer  : fre@freux.fr
-}
module Nrm.Types.Container
  ( ContainerUUID (..)
  , nextContainerUUID
  )
where

import Data.UUID
import Data.UUID.V1
import Protolude

newtype ContainerUUID = ContainerUUID UUID
  deriving (Eq, Ord)

nextContainerUUID :: IO (Maybe ContainerUUID)
nextContainerUUID = fmap ContainerUUID <$> nextUUID
