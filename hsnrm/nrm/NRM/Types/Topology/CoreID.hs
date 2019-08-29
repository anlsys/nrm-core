{-|
Module      : NRM.Types.Topology.CoreID
Copyright   : (c) UChicago Argonne, 2019
License     : BSD3
Maintainer  : fre@freux.fr
-}
module NRM.Types.Topology.CoreID
  ( CoreID (..)
  )
where

import Data.Aeson
import Data.Either
import Data.MessagePack
import Protolude
import Refined
import Refined.Orphan.Aeson ()
import Prelude (fail)
import NRM.Classes.Topology

-- | A CPU Core OS identifier.
newtype CoreID = CoreID (Refined Positive Int)
  deriving (Show, Eq, Ord, Generic, ToJSONKey, ToJSON, FromJSON, FromJSONKey)

instance MessagePack CoreID where

  toObject (CoreID x) = toObject (unrefine x)

  fromObject x =
    (fromObject x <&> refine) >>= \case
      Right r -> return $ CoreID r
      Left _ -> fail "Couldn't refine PackageID during MsgPack conversion"

instance ToHwlocType CoreID where

  getType _ = "Core"

instance IdFromString CoreID where

  idFromString s = CoreID <$> readMaybe ("Refined " <> s)
