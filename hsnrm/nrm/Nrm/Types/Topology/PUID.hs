{-|
Module      : Nrm.Types.Topology.PUID
Copyright   : (c) UChicago Argonne, 2019
License     : BSD3
Maintainer  : fre@freux.fr
-}
module Nrm.Types.Topology.PUID
  ( PUID (..)
  )
where

import Data.Aeson
import Data.Either
import qualified Data.Map as DM
import Data.MessagePack
import Protolude
import Refined
import Refined.Orphan.Aeson ()
import Prelude (String, fail)
import Nrm.Classes.Topology

-- | A Processing Unit OS identifier.
newtype PUID = PUID (Refined NonNegative Int)
  deriving (Eq, Ord, Show, Generic, ToJSON, FromJSON, ToJSONKey, FromJSONKey)

-- | reading from hwloc XML data
instance MessagePack PUID where

  toObject (PUID x) = toObject (unrefine x)

  fromObject x =
    (fromObject x <&> refine) >>= \case
      Right r -> return $ PUID r
      Left _ -> fail "Couldn't refine PackageID during MsgPack conversion"

instance IdFromString PUID where

  idFromString s = PUID <$> readMaybe ("Refined " <> s)

instance ToHwlocType PUID where

  getType _ = "PU"
