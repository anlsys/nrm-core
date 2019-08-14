{-|
Module      : Nrm.Types.Topology
Copyright   : (c) UChicago Argonne, 2019
License     : BSD3
Maintainer  : fre@freux.fr
-}
module Nrm.Types.Topology
  ( CoreID
  , PUID
  , PackageID
  , Topology (..)
  , IdFromString (..)
  , ToHwlocType (..)
  )
where

import Data.Either
import Data.MessagePack
import Protolude
import Refined
import Prelude (String, fail)

-- | Nnm's internal statically typed topology representation
data Topology
  = Topology
      { puIDs :: [PUID]
      , coreIDs :: [CoreID]
      , packageIDs :: [PackageID]
      }
  deriving (Generic)

deriving instance MessagePack Topology

-- | A CPU Core OS identifier.
newtype CoreID = CoreID (Refined Positive Int)
  deriving (Show)

instance MessagePack CoreID where

  toObject (CoreID x) = toObject (unrefine x)

  fromObject x =
    (fromObject x <&> refine) >>= \case
      Right r -> return $ CoreID r
      Left _ -> fail "Couldn't refine PackageID during MsgPack conversion"

-- | A Processing Unit OS identifier.
newtype PUID = PUID (Refined NonNegative Int)
  deriving (Show)

instance MessagePack PUID where

  toObject (PUID x) = toObject (unrefine x)

  fromObject x =
    (fromObject x <&> refine) >>= \case
      Right r -> return $ PUID r
      Left _ -> fail "Couldn't refine PackageID during MsgPack conversion"

-- | A Package OS identifier.
newtype PackageID = PackageID (Refined NonNegative Int)
  deriving (Show, Generic)

instance MessagePack PackageID where

  toObject (PackageID x) = toObject (unrefine x)

  fromObject x =
    (fromObject x <&> refine) >>= \case
      Right r -> return $ PackageID r
      Left _ -> fail "Couldn't refine PackageID during MsgPack conversion"

{-deriving instance MessagePack (Refined NonNegative Int) where-}

-- | reading from hwloc XML data
class IdFromString a where

  idFromString :: String -> Maybe a

-- | translating to hwloc XML "type" field.
class ToHwlocType a where

  getType :: Proxy a -> Text

instance IdFromString CoreID where

  idFromString s = CoreID <$> readMaybe ("Refined " <> s)

instance IdFromString PUID where

  idFromString s = PUID <$> readMaybe ("Refined " <> s)

instance IdFromString PackageID where

  idFromString s = PackageID <$> readMaybe ("Refined " <> s)

instance ToHwlocType PUID where

  getType _ = "PU"

instance ToHwlocType CoreID where

  getType _ = "Core"

instance ToHwlocType PackageID where

  getType _ = "Package"
