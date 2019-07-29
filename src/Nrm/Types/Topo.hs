{-|
Module      : Nrm.Types.Topo
Description : Topology related types
Copyright   : (c) UChicago Argonne, 2019
License     : BSD3
Maintainer  : fre@freux.fr
-}
module Nrm.Types.Topo
  ( CoreId
  , PUId
  , PackageId
  , IdFromString (..)
  , ToHwlocType (..)
  )
where

import Protolude
import Refined
import Prelude (String)

-- | A CPU Core OS identifier.
newtype CoreId = CoreId (Refined Positive Int)
  deriving (Show)

-- | A Processing Unit OS identifier.
newtype PUId = PUId (Refined NonNegative Int)
  deriving (Show)

-- | A Package OS identifier.
newtype PackageId = PackageId (Refined NonNegative Int)
  deriving (Show)

-- | reading from hwloc XML data
class IdFromString a where

  idFromString :: String -> Maybe a

-- | translating to hwloc XML "type" field.
class ToHwlocType a where

  getType :: Proxy a -> Text

instance IdFromString CoreId where

  idFromString s = CoreId <$> readMaybe ("Refined " <> s)

instance IdFromString PUId where

  idFromString s = PUId <$> readMaybe ("Refined " <> s)

instance IdFromString PackageId where

  idFromString s = PackageId <$> readMaybe ("Refined " <> s)

instance ToHwlocType PUId where

  getType _ = "PU"

instance ToHwlocType CoreId where

  getType _ = "Core"

instance ToHwlocType PackageId where

  getType _ = "Package"
