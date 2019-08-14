{-|
Module      : Nrm.Types.Process
Copyright   : (c) UChicago Argonne, 2019
License     : BSD3
Maintainer  : fre@freux.fr
-}
module Nrm.Types.Process
  ( ProcessID (..)
  )
where

import qualified Data.Aeson as A
import Data.Aeson
import Data.MessagePack
import Data.JSON.Schema
import Protolude
import qualified System.Posix.Types as P

newtype ProcessID = ProcessID P.CPid
  deriving (Eq, Ord, Show, Generic)

instance ToJSON ProcessID where

  toJSON (ProcessID x)= toJSON (fromIntegral x :: Int)

instance FromJSON ProcessID where

  parseJSON = fmap (ProcessID . P.CPid) . parseJSON

instance JSONSchema ProcessID where

  schema Proxy = schema (Proxy :: Proxy Int)

instance MessagePack ProcessID where

  toObject (ProcessID x) = toObject (fromIntegral x :: Int)

  fromObject x = ProcessID . P.CPid <$> fromObject x
