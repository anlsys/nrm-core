{-|
Module      : Nrm.Types.Process
Copyright   : (c) UChicago Argonne, 2019
License     : BSD3
Maintainer  : fre@freux.fr
-}
module Nrm.Types.Process
  ( Cmd (..)
  , CmdSpec (..)
  , mkCmd
  , ProcessID (..)
  , ThreadID (..)
  , TaskID (..)
  , CmdID (..)
  , Command (..)
  , Arguments (..)
  , Arg (..)
  , Env (..)
  , nextCmdID
  , toText
  , fromText
  )
where

import qualified Data.Aeson as A
import Data.Aeson
import Data.JSON.Schema
import Data.MessagePack
import Data.String (IsString (..))
import qualified Data.UUID as U
import Data.UUID.V1
import Generics.Generic.Aeson
import Protolude
import qualified System.Posix.Types as P
import Prelude (fail)

data CmdSpec
  = CmdSpec
      { cmd :: Command
      , args :: Arguments
      , env :: Env
      }
  deriving (Show, Generic, MessagePack, FromJSON, ToJSON)

newtype Cmd
  = Cmd
      { spec :: CmdSpec
      }
  deriving (Show, Generic, MessagePack, FromJSON, ToJSON)

instance JSONSchema CmdSpec where

  schema = gSchema

instance JSONSchema Cmd where

  schema = gSchema

mkCmd :: CmdSpec -> Cmd
mkCmd s = Cmd {spec = s}

newtype TaskID = TaskID Int
  deriving (Eq, Ord, Show, Read, Generic, MessagePack)

newtype ThreadID = ThreadID Int
  deriving (Eq, Ord, Show, Read, Generic, MessagePack)

newtype ProcessID = ProcessID P.CPid
  deriving (Eq, Ord, Show, Read, Generic)

newtype Arg = Arg Text
  deriving (Show, Generic, MessagePack)

newtype Command = Command Text
  deriving (Show, Generic, MessagePack)

newtype Arguments = Arguments [Arg]
  deriving (Show, Generic, MessagePack)

newtype Env = Env [(Text, Text)]
  deriving (Show, Generic, MessagePack)

instance ToJSON Env where

  toJSON = gtoJson

instance FromJSON Env where

  parseJSON = gparseJson

instance JSONSchema Env where

  schema = gSchema

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

newtype CmdID = CmdID U.UUID
  deriving (Show, Eq, Ord, Generic, ToJSONKey, FromJSONKey)

instance IsString CmdID where

  fromString x = fromMaybe (panic "couldn't decode cmdID in FromString instance") (decode $ toS x)

nextCmdID :: IO (Maybe CmdID)
nextCmdID = fmap CmdID <$> nextUUID

parseCmdID :: Text -> Maybe CmdID
parseCmdID = fmap CmdID <$> U.fromText

toText :: CmdID -> Text
toText (CmdID u) = U.toText u

fromText :: Text -> Maybe CmdID
fromText = fmap CmdID <$> U.fromText

instance ToJSON CmdID where

  toJSON = gtoJson

instance FromJSON CmdID where

  parseJSON = gparseJson

instance JSONSchema CmdID where

  schema Proxy = schema (Proxy :: Proxy Text)

instance MessagePack CmdID where

  toObject (CmdID c) = toObject $ U.toText c

  fromObject x =
    fromObject x >>= \y ->
      case parseCmdID y of
        Nothing -> fail "Couldn't parse CmdID"
        Just t -> return t
