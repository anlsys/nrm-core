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
  , PkgId
  , IdFromString (..)
  , ToHwlocType (..)
  )
where

import Protolude
import Prelude (String)

newtype CoreId = CoreId Integer
  deriving (Show)

newtype PUId = PUId Integer
  deriving (Show)

newtype PkgId = PkgId Integer
  deriving (Read)

class IdFromString a where

  idFromString :: String -> Maybe a

instance IdFromString CoreId where

  idFromString s = CoreId <$> readMaybe s

instance IdFromString PUId where

  idFromString s = PUId <$> readMaybe s

class ToHwlocType a where

  getType :: Proxy a -> Text

instance ToHwlocType PUId where

  getType _ = "PU"

instance ToHwlocType CoreId where

  getType _ = "Core"
