{-# LANGUAGE DerivingVia #-}

{-|
Module      : NRM.Types.CmdID
Copyright   : (c) UChicago Argonne, 2019
License     : BSD3
Maintainer  : fre@freux.fr
-}
module NRM.Types.CmdID
  ( CmdID (..)
  , NRM.Types.CmdID.toText
  , NRM.Types.CmdID.fromText
  , nextCmdID
  )
where

import Data.Aeson as A
import Data.Data
import Data.JSON.Schema
import Data.MessagePack
import Data.String (IsString (..))
import qualified Data.UUID as U
import Data.UUID.V1 (nextUUID)
import Dhall hiding (field)
import NRM.Classes.Messaging
import NRM.Orphans.ExitCode ()
import NRM.Orphans.UUID ()
import Protolude

newtype CmdID = CmdID U.UUID
  deriving (Show, Eq, Ord, Generic, Data, ToJSONKey, FromJSONKey, MessagePack)
  deriving (JSONSchema, ToJSON, FromJSON) via GenericJSON CmdID

instance IsString CmdID where

  fromString x = fromMaybe (panic "couldn't decode cmdID in FromString instance") (A.decode $ toS x)

nextCmdID :: IO (Maybe CmdID)
nextCmdID = fmap CmdID <$> nextUUID

toText :: CmdID -> Text
toText (CmdID u) = U.toText u

fromText :: Text -> Maybe CmdID
fromText = fmap CmdID <$> U.fromText

