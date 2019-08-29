{-|
Module      : NRM.Types.Topology.PackageID
Copyright   : (c) UChicago Argonne, 2019
License     : BSD3
Maintainer  : fre@freux.fr
-}
module NRM.Types.Topology.PackageID
  ( PackageID (..)
  )
where

import Data.Aeson
import Data.Either
import Data.MessagePack
import NRM.Classes.Topology
import Protolude
import Refined
import Refined.Orphan.Aeson ()
import Prelude (fail)

-- | A Package OS identifier.
newtype PackageID = PackageID (Refined NonNegative Int)
  deriving (Eq, Ord, Show, Generic, FromJSONKey, ToJSONKey, ToJSON, FromJSON)

-- MessagePack instances
instance MessagePack PackageID where

  toObject (PackageID x) = toObject (unrefine x)

  fromObject x =
    (fromObject x <&> refine) >>= \case
      Right r -> return $ PackageID r
      Left _ -> fail "Couldn't refine PackageID during MsgPack conversion"

instance IdFromString PackageID where

  idFromString s = PackageID <$> readMaybe ("Refined " <> s)

instance ToHwlocType PackageID where

  getType _ = "Package"
