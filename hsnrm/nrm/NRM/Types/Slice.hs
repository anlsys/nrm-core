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
import Data.Data
import Data.Generics.Product
import Data.JSON.Schema
import Data.MessagePack
import qualified Data.UUID as U (UUID, fromText, toText)
import Data.UUID.V1
import NRM.Classes.Messaging
import NRM.Classes.Sensors
import NRM.Types.Cmd (Cmd (..), CmdCore (..), CmdID (..))
import NRM.Types.LMap as LM
import Protolude

-- | NRM's internal view of the state of a slice.
data Slice
  = Slice
      { -- | map of running commands
        cmds :: LMap CmdID Cmd
      , -- | map of commands awaiting to be registered as running by the runtime
        awaiting :: LMap CmdID CmdCore
      }
  deriving (Show, Generic, Data, MessagePack)
  deriving (ToJSON, FromJSON, JSONSchema) via GenericJSON Slice

-- | Constructor for an empty slice.
emptySlice :: Slice
emptySlice = Slice
  { cmds = LM.fromList []
  , awaiting = LM.fromList []
  }

-- | Insert a running command in a slice (with replace)
insertCmd :: CmdID -> Cmd -> Slice -> Slice
insertCmd cmdID cmd slice = slice {cmds = LM.insert cmdID cmd (cmds slice)}

data SliceID = SliceID U.UUID | Name Text
  deriving (Show, Eq, Ord, Generic, Data, FromJSONKey, ToJSONKey, MessagePack)
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

-- Sensor instance
instance Sensors (SliceID, Slice) where

  passiveSensors (_, slice) = passiveSensors (cmds slice)

  activeSensors (_, slice) = activeSensors (cmds slice)

instance AdjustSensors (SliceID, Slice) where

  adjust sensorID range (sid, s) =
    ( sid
    , runIdentity $
      constraints'
        @AdjustSensors
        (pure . adjust sensorID range)
        s
    )
