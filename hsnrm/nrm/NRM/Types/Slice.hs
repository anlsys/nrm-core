{-# LANGUAGE DerivingVia #-}

{-|
Module      : NRM.Types.Slice
Copyright   : (c) UChicago Argonne, 2019
License     : BSD3
Maintainer  : fre@freux.fr
-}
module NRM.Types.Slice
  ( Slice (..)
  , emptySlice
  , insertCmd
  , SliceID (..)
  , nextSliceID
  , parseSliceID
  , toText
  )
where

import Data.Aeson
import Data.JSON.Schema
import qualified Data.Map as DM
import Data.MessagePack
import qualified Data.UUID as U (UUID, fromText, toText)
import Data.UUID.V1
import NRM.Classes.Messaging
import NRM.Types.DownstreamClient
import NRM.Types.Process (Cmd (..), CmdCore (..), CmdID (..))
import Protolude

-- | NRM's internal view of the state of a slice.
data Slice
  = Slice
      { -- | map of running commands
        cmds :: DM.Map CmdID Cmd
      , -- | map of commands awaiting to be registered as running by the runtime
        awaiting :: DM.Map CmdID CmdCore
      , -- | map of downstream "command-level" clients for this slice
        downstreamCmds :: Map DownstreamCmdID DownstreamCmd
      , -- | map of downstream "thread-level" clients for this slice
        downstreamThreads :: Map DownstreamThreadID DownstreamThread
      }
  deriving (Show, Generic, MessagePack)
  deriving (ToJSON, FromJSON, JSONSchema) via GenericJSON Slice

-- | Constructor for an empty slice.
emptySlice :: Slice
emptySlice = Slice
  { cmds = DM.fromList []
  , awaiting = DM.fromList []
  , downstreamCmds = DM.fromList []
  , downstreamThreads = DM.fromList []
  }

-- | Insert a running command in a slice (with replace)
insertCmd :: CmdID -> Cmd -> Slice -> Slice
insertCmd cmdID cmd slice = slice {cmds = DM.insert cmdID cmd (cmds slice)}

data SliceID = SliceID U.UUID | Name Text
  deriving (Show, Eq, Ord, Generic, FromJSONKey, ToJSONKey)
  deriving (ToJSON, FromJSON) via GenericJSON SliceID

nextSliceID :: IO (Maybe SliceID)
nextSliceID = fmap SliceID <$> nextUUID

parseSliceID :: Text -> SliceID
parseSliceID t = case U.fromText t of
  Just x -> SliceID x
  Nothing -> Name t

toText :: SliceID -> Text
toText (SliceID u) = U.toText u
toText (Name n) = n

instance JSONSchema SliceID where

  schema Proxy = schema (Proxy :: Proxy Text)

instance MessagePack SliceID where

  toObject (SliceID c) = toObject $ U.toText c
  toObject (Name c) = toObject c

  fromObject x = fromObject x <&> parseSliceID
