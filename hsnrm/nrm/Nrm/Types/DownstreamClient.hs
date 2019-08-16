{-|
Module      : Nrm.Types.DownstreamClient
Copyright   : (c) UChicago Argonne, 2019
License     : BSD3
Maintainer  : fre@freux.fr
-}
module Nrm.Types.DownstreamClient
  ( DownstreamID (..)
  )
where

import qualified Data.Aeson as A
import Data.Aeson
import Data.JSON.Schema
import Data.MessagePack
import Generics.Generic.Aeson
import Nrm.Types.Process as P
import Protolude

data DownstreamID = DownstreamID P.ProcessID P.ThreadID
  deriving (Eq, Ord, Show, Generic)

instance ToJSON DownstreamID where

  toJSON = gtoJson

instance FromJSON DownstreamID where

  parseJSON = gparseJson

instance JSONSchema DownstreamID where

  schema Proxy = schema (Proxy :: Proxy Text)

deriving instance MessagePack DownstreamID
