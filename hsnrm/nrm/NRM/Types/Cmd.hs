{-# LANGUAGE DerivingVia #-}

{-|
Module      : NRM.Types.Cmd
Copyright   : (c) UChicago Argonne, 2019
License     : BSD3
Maintainer  : fre@freux.fr
-}
module NRM.Types.Cmd
  ( Cmd (..)
  , CmdCore (..)
  , CmdSpec (..)
  , mkCmd
  , registerPID
  , TaskID (..)
  , CmdID (..)
  , Command (..)
  , Arguments (..)
  , Arg (..)
  , Env (..)
  , nextCmdID
  , toText
  , fromText
  , wrapCmd
  )
where

import Data.Aeson as A
import Data.JSON.Schema
import Data.Map as DM
import Data.MessagePack
import Data.String (IsString (..))
import qualified Data.UUID as U
import Data.UUID.V1 (nextUUID)
import Dhall
import NRM.Classes.Messaging
import NRM.Orphans.ExitCode ()
import NRM.Orphans.UUID ()
import qualified NRM.Types.DownstreamCmdClient as DC
import NRM.Types.Process
import qualified NRM.Types.UpstreamClient as UC
import Protolude

data CmdSpec
  = CmdSpec
      { cmd :: Command
      , args :: Arguments
      , env :: Env
      }
  deriving (Show, Generic, MessagePack)
  deriving (JSONSchema, ToJSON, FromJSON) via GenericJSON CmdSpec

data CmdCore
  = CmdCore
      { cmdPath :: Command
      , arguments :: Arguments
      , upstreamClientID :: Maybe UC.UpstreamClientID
      }
  deriving (Show, Generic, MessagePack)
  deriving (JSONSchema, ToJSON, FromJSON) via GenericJSON CmdCore

data Cmd
  = Cmd
      { cmdCore :: CmdCore
      , pid :: ProcessID
      , processState :: ProcessState
      , downstreamCmds :: Map DC.DownstreamCmdID DC.DownstreamCmd
      }
  deriving (Show, Generic, MessagePack)
  deriving (JSONSchema, ToJSON, FromJSON) via GenericJSON Cmd

mkCmd :: CmdSpec -> Maybe UC.UpstreamClientID -> CmdCore
mkCmd s clientID = CmdCore {cmdPath = cmd s, arguments = args s, upstreamClientID = clientID}

registerPID :: CmdCore -> ProcessID -> Cmd
registerPID c pid = Cmd {cmdCore = c, processState = blankState, downstreamCmds = DM.empty, ..}

newtype TaskID = TaskID Int
  deriving (Eq, Ord, Show, Read, Generic, MessagePack)
  deriving (JSONSchema, ToJSON, FromJSON) via GenericJSON TaskID

newtype Arg = Arg Text
  deriving (Show, Generic, MessagePack)
  deriving (JSONSchema, ToJSON, FromJSON) via GenericJSON Arg

instance StringConv Arg Text where

  strConv _ (Arg x) = toS x

newtype Command = Command Text
  deriving (Show, Eq, Generic, MessagePack)
  deriving (JSONSchema, ToJSON, FromJSON) via GenericJSON Command
  deriving (IsString, Interpret, Inject) via Text

instance StringConv Command Text where

  strConv _ (Command x) = toS x

newtype Arguments = Arguments [Arg]
  deriving (Show, Generic, MessagePack)
  deriving (JSONSchema, ToJSON, FromJSON) via GenericJSON Arguments

newtype Env = Env [(Text, Text)]
  deriving (Show, Generic, MessagePack)
  deriving (JSONSchema, ToJSON, FromJSON) via GenericJSON Env
  deriving (Semigroup, Monoid) via [(Text, Text)]

newtype CmdID = CmdID U.UUID
  deriving (Show, Eq, Ord, Generic, ToJSONKey, FromJSONKey, MessagePack)
  deriving (JSONSchema, ToJSON, FromJSON) via GenericJSON CmdID

instance IsString CmdID where

  fromString x = fromMaybe (panic "couldn't decode cmdID in FromString instance") (A.decode $ toS x)

nextCmdID :: IO (Maybe CmdID)
nextCmdID = fmap CmdID <$> nextUUID

toText :: CmdID -> Text
toText (CmdID u) = U.toText u

fromText :: Text -> Maybe CmdID
fromText = fmap CmdID <$> U.fromText

wrapCmd :: Command -> (Command, Arguments) -> (Command, Arguments)
wrapCmd c (Command a, Arguments as) = (c, Arguments $ Arg a : as)
