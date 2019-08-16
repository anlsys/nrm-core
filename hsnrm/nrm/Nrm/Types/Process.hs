{-|
Module      : Nrm.Types.Process
Copyright   : (c) UChicago Argonne, 2019
License     : BSD3
Maintainer  : fre@freux.fr
-}
module Nrm.Types.Process
  ( ProcessID (..)
  , ThreadID(..)
  , TaskID(..)
  , Command (..)
  , Arguments (..)
  , Arg (..)
  )
where

import qualified Data.Aeson as A
import Data.Aeson
import Data.JSON.Schema
import Data.MessagePack
import Generics.Generic.Aeson
import Protolude
import qualified System.Posix.Types as P

newtype TaskID = TaskID Int
  deriving (Eq, Ord, Show, Read, Generic)

newtype ThreadID = ThreadID Int
  deriving (Eq, Ord, Show, Read, Generic)

newtype ProcessID = ProcessID P.CPid
  deriving (Eq, Ord, Show, Read, Generic)

newtype Arg = Arg Text
  deriving (Show, Generic)

newtype Command = Command Text
  deriving (Show, Generic)

newtype Arguments = Arguments [Arg]
  deriving (Show, Generic)

deriving instance MessagePack Arguments

deriving instance MessagePack Command

deriving instance MessagePack Arg

deriving instance MessagePack ThreadID
deriving instance MessagePack TaskID

instance ToJSON ThreadID where

  toJSON = gtoJson

instance FromJSON ThreadID where

  parseJSON = gparseJson

instance JSONSchema ThreadID where

  schema = gSchema


instance ToJSON TaskID where

  toJSON = gtoJson

instance FromJSON TaskID where

  parseJSON = gparseJson

instance JSONSchema TaskID where

  schema = gSchema

instance MessagePack ProcessID where

  toObject (ProcessID x) = toObject (fromIntegral x :: Int)

  fromObject x = ProcessID . P.CPid <$> fromObject x

instance ToJSON ProcessID where

  toJSON (ProcessID x) = toJSON (fromIntegral x :: Int)

instance FromJSON ProcessID where

  parseJSON = fmap (ProcessID . P.CPid) . parseJSON

instance JSONSchema ProcessID where

  schema Proxy = schema (Proxy :: Proxy Int)

instance ToJSON Command where

  toJSON = gtoJson

instance FromJSON Command where

  parseJSON = gparseJson

instance JSONSchema Command where

  schema = gSchema

instance ToJSON Arguments where

  toJSON = gtoJson

instance FromJSON Arguments where

  parseJSON = gparseJson

instance JSONSchema Arguments where

  schema = gSchema

instance ToJSON Arg where

  toJSON = gtoJson

instance FromJSON Arg where

  parseJSON = gparseJson

instance JSONSchema Arg where

  schema = gSchema
