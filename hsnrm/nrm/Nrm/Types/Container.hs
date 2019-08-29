{-|
Module      : Nrm.Types.Container
Copyright   : (c) UChicago Argonne, 2019
License     : BSD3
Maintainer  : fre@freux.fr
-}
module Nrm.Types.Container
  ( Container (..)
  , emptyContainer
  , insertCmd
  , ContainerID (..)
  , nextContainerID
  , parseContainerID
  , toText
  )
where

import qualified Data.Aeson as A
import Data.Aeson
import Data.JSON.Schema
import qualified Data.Map as DM
import Data.MessagePack
import qualified Data.UUID as U (UUID, fromText, toText)
import Data.UUID.V1
import Generics.Generic.Aeson
import Nrm.Types.DownstreamClient
import Nrm.Types.Process (Cmd (..), CmdCore (..), CmdID (..))
import Protolude

-- | NRM's internal view of the state of a container.
data Container
  = Container
      { -- | map of running commands
        cmds :: DM.Map CmdID Cmd
      , -- | map of commands awaiting to be registered as running by the runtime
        awaiting :: DM.Map CmdID CmdCore
      , -- | map of downstream "command-level" clients for this container
        downstreamCmds :: Map DownstreamCmdID DownstreamCmd
      , -- | map of downstream "thread-level" clients for this container
        downstreamThreads :: Map DownstreamThreadID DownstreamThread
      }
  deriving (Show, Generic, MessagePack, ToJSON, FromJSON)

-- | Constructor for an empty container.
emptyContainer :: Container
emptyContainer = Container
  { cmds = DM.fromList []
  , awaiting = DM.fromList []
  , downstreamCmds = DM.fromList []
  , downstreamThreads = DM.fromList []
  }

-- | Insert a running command in a container (with replace)
insertCmd :: CmdID -> Cmd -> Container -> Container
insertCmd cmdID cmd container = container {cmds = DM.insert cmdID cmd (cmds container)}

data ContainerID = ContainerID U.UUID | Name Text
  deriving (Show, Eq, Ord, Generic, FromJSONKey, ToJSONKey)

nextContainerID :: IO (Maybe ContainerID)
nextContainerID = fmap ContainerID <$> nextUUID

parseContainerID :: Text -> ContainerID
parseContainerID t = case U.fromText t of
  Just x -> ContainerID x
  Nothing -> Name t

toText :: ContainerID -> Text
toText (ContainerID u) = U.toText u
toText (Name n) = n

instance ToJSON ContainerID where

  toJSON = gtoJson

instance FromJSON ContainerID where

  parseJSON = gparseJson

instance JSONSchema ContainerID where

  schema Proxy = schema (Proxy :: Proxy Text)

instance MessagePack ContainerID where

  toObject (ContainerID c) = toObject $ U.toText c
  toObject (Name c) = toObject c

  fromObject x = fromObject x <&> parseContainerID

instance JSONSchema Container where

  schema = gSchema
