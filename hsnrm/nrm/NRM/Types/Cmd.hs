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
  , NRM.Types.Cmd.toText
  , NRM.Types.Cmd.fromText
  , wrapCmd
  , addDownstreamCmdClient
  , removeDownstreamCmdClient
  )
where

import Control.Lens
import Data.Aeson as A
import Data.Data
import Data.Generics.Product
import Data.JSON.Schema
import Data.MessagePack
import Data.String (IsString (..))
import qualified Data.UUID as U
import Data.UUID.V1 (nextUUID)
import Dhall hiding (field)
import LMap.Map as LM
import LensMap.Core
import NRM.Classes.Messaging
import NRM.Orphans.ExitCode ()
import NRM.Orphans.UUID ()
import NRM.Types.DownstreamCmd
import NRM.Types.DownstreamCmdID
import NRM.Types.Manifest as Manifest
import NRM.Types.Process
import NRM.Types.Sensor
import qualified NRM.Types.UpstreamClient as UC
import Protolude

data CmdSpec
  = CmdSpec
      { cmd :: Command
      , args :: Arguments
      , env :: Env
      }
  deriving (Show, Generic, Data, MessagePack)
  deriving (JSONSchema, ToJSON, FromJSON) via GenericJSON CmdSpec

data CmdCore
  = CmdCore
      { cmdPath :: Command
      , arguments :: Arguments
      , upstreamClientID :: Maybe UC.UpstreamClientID
      , manifest :: Manifest
      }
  deriving (Show, Generic, Data, MessagePack)
  deriving (JSONSchema, ToJSON, FromJSON) via GenericJSON CmdCore

data Cmd
  = Cmd
      { cmdCore :: CmdCore
      , pid :: ProcessID
      , processState :: ProcessState
      , downstreamCmds :: LM.Map DownstreamCmdID DownstreamCmd
      }
  deriving (Show, Generic, Data, MessagePack)
  deriving (JSONSchema, ToJSON, FromJSON) via GenericJSON Cmd

mkCmd :: CmdSpec -> Manifest -> Maybe UC.UpstreamClientID -> CmdCore
mkCmd s manifest clientID = CmdCore {cmdPath = cmd s, arguments = args s, upstreamClientID = clientID, ..}

registerPID :: CmdCore -> ProcessID -> Cmd
registerPID c pid = Cmd
  { cmdCore = c
  , processState = blankState
  , downstreamCmds = LM.empty
  , ..
  }

addDownstreamCmdClient
  :: Cmd
  -> DownstreamCmdID
  -> Maybe Cmd
addDownstreamCmdClient Cmd {..} downstreamCmdClientID =
  cmdCore & manifest & Manifest.app & Manifest.perfwrapper & \case
    PerfwrapperDisabled -> Nothing
    Perfwrapper pw ->
      Just $ Cmd
        { downstreamCmds = LM.insert downstreamCmdClientID
            ( DownstreamCmd
              (Manifest.perfLimit pw)
              (Manifest.perfFreq pw)
            )
            downstreamCmds
        , ..
        }

removeDownstreamCmdClient :: Cmd -> DownstreamCmdID -> Cmd
removeDownstreamCmdClient Cmd {..} downstreamCmdClientID = Cmd
  { downstreamCmds = LM.delete downstreamCmdClientID downstreamCmds
  , ..
  }

newtype TaskID = TaskID Int
  deriving (Eq, Ord, Show, Read, Generic, Data, MessagePack)
  deriving (JSONSchema, ToJSON, FromJSON) via GenericJSON TaskID

newtype Arg = Arg Text
  deriving (Show, Generic, Data, MessagePack)
  deriving (JSONSchema, ToJSON, FromJSON) via GenericJSON Arg

instance StringConv Arg Text where

  strConv _ (Arg x) = toS x

newtype Command = Command Text
  deriving (Show, Eq, Generic, Data, MessagePack)
  deriving (JSONSchema, ToJSON, FromJSON) via GenericJSON Command
  deriving (IsString, Interpret, Inject) via Text

instance StringConv Command Text where

  strConv _ (Command x) = toS x

newtype Arguments = Arguments [Arg]
  deriving (Show, Generic, Data, MessagePack)
  deriving (JSONSchema, ToJSON, FromJSON) via GenericJSON Arguments

newtype Env = Env [(Text, Text)]
  deriving (Show, Generic, Data, MessagePack)
  deriving (JSONSchema, ToJSON, FromJSON) via GenericJSON Env
  deriving (Semigroup, Monoid) via [(Text, Text)]

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

wrapCmd :: Command -> (Command, Arguments) -> (Command, Arguments)
wrapCmd c (Command a, Arguments as) = (c, Arguments $ Arg a : as)

instance HasLensMap (CmdID, Cmd) ActiveSensorKey ActiveSensor where

  lenses (_cmdID, cmd) =
    addPath (_2 . field @"downstreamCmds") <$> lenses (downstreamCmds cmd)
