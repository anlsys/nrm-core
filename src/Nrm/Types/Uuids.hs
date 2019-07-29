{-|
Module      : Nrm.Types.Uuids
Description : Uuids
Copyright   : (c) UChicago Argonne, 2019
License     : BSD3
Maintainer  : fre@freux.fr
-}
module Nrm.Types.Uuids
  ( ContainerUUID(..)
  , ApplicationUUID(..)  )
where

import Data.UUID

newtype ContainerUUID = ContainerUUID UUID

newtype ApplicationUUID = ApplicationUUID UUID
