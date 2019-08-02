{-|
Module      : Nrm.Types.Containers
Description : Containers
Copyright   : (c) UChicago Argonne, 2019
License     : BSD3
Maintainer  : fre@freux.fr
-}
module Nrm.Types.Containers
  ( ContainerUUID (..)
  )
where

import Data.UUID

newtype ContainerUUID = ContainerUUID UUID
