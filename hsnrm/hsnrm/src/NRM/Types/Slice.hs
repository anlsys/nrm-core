{-# LANGUAGE DerivingVia #-}

-- |
-- Module      : NRM.Types.Slice
-- Copyright   : (c) UChicago Argonne, 2019
-- License     : BSD3
-- Maintainer  : fre@freux.fr
module NRM.Types.Slice
  ( Slice (..),
    emptySlice,
    SliceID (..),
    nextSliceID,
    parseSliceID,
    toText,
  )
where

import Control.Lens
import Data.Aeson
import Data.Generics.Labels ()
import Data.JSON.Schema
import Data.Map as M
import Data.MessagePack
import qualified Data.UUID as U (UUID, fromText, toText)
import Data.UUID.V1
import LensMap.Core
import NRM.Classes.Messaging
import NRM.Types.Actuator
import NRM.Types.Cmd (Cmd (..), CmdCore (..))
import NRM.Types.CmdID (CmdID (..))
import NRM.Types.Sensor
import Protolude

-- | NRM's internal view of the state of a slice.
data Slice
  = Slice
      { -- | map of running commands
        cmds :: M.Map CmdID Cmd,
        -- | map of commands awaiting to be registered as running by the runtime
        awaiting :: M.Map CmdID CmdCore
      }
  deriving (Show, Generic, MessagePack)
  deriving (ToJSON, FromJSON, JSONSchema) via GenericJSON Slice

-- | Constructor for an empty slice.
emptySlice :: Slice
emptySlice = Slice
  { cmds = M.fromList [],
    awaiting = M.fromList []
  }

data SliceID = SliceID U.UUID | Name Text
  deriving (Show, Eq, Ord, Generic, FromJSONKey, ToJSONKey, MessagePack)
  deriving (ToJSON, FromJSON, JSONSchema) via GenericJSON SliceID

nextSliceID :: IO (Maybe SliceID)
nextSliceID = fmap SliceID <$> nextUUID

parseSliceID :: Text -> SliceID
parseSliceID t = case U.fromText t of
  Just x -> SliceID x
  Nothing -> Name t

toText :: SliceID -> Text
toText (SliceID u) = U.toText u
toText (Name n) = n

instance HasLensMap (SliceID, Slice) ActiveSensorKey ActiveSensor where
  lenses (_sliceID, slice) =
    addPath (_2 . #cmds) <$> lenses (cmds slice)

instance HasLensMap (SliceID, Slice) ActuatorKey Actuator where
  lenses (_sliceID, slice) =
    addPath (_2 . #cmds) <$> lenses (cmds slice)
