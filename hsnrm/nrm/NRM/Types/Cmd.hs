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
  , addDownstreamCmdClient
  , removeDownstreamCmdClient
  )
where

import CPD.Core as CPD
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
import NRM.Classes.Actuators
import NRM.Classes.Messaging
import NRM.Classes.Sensors
import NRM.Orphans.ExitCode ()
import NRM.Orphans.UUID ()
import qualified NRM.Types.DownstreamCmd as DC
import NRM.Types.LMap as LM
import NRM.Types.Manifest as Manifest
import NRM.Types.Process
import NRM.Types.Sensor
import NRM.Types.Units as Units
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
      , downstreamCmds :: LMap DC.DownstreamCmdID DC.DownstreamCmd
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
  -> DC.DownstreamCmdID
  -> Maybe Cmd
addDownstreamCmdClient Cmd {..} downstreamCmdClientID =
  cmdCore & manifest & Manifest.app & Manifest.perfwrapper & \case
    PerfwrapperDisabled -> Nothing
    Perfwrapper pw ->
      Just $ Cmd
        { downstreamCmds = LM.insert downstreamCmdClientID
            ( DC.DownstreamCmd
              (DC.toSensorID downstreamCmdClientID)
              (Manifest.perfLimit pw)
            )
            downstreamCmds
        , ..
        }

removeDownstreamCmdClient :: Cmd -> DC.DownstreamCmdID -> Cmd
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

deriving via (NoActuators (CmdID, Cmd)) instance Actuators (CmdID, Cmd)

instance Sensors (CmdID, Cmd) where

  passiveSensors _ = LM.empty

  activeSensors (cmdID, Cmd {..}) =
    LM.fromList
      ( LM.elems downstreamCmds <&> \dc ->
        ( DC.id dc
        , ActiveSensor
          { activeTags = [Tag "perf"]
          , activeSource = Source $ show cmdID
          , activeRange = (0, 1)
          , maxFrequency = ratelimit $ monitoring $ app $ manifest cmdCore
          , process = identity
          }
        )
      )

instance AdjustSensors (CmdID, Cmd) where

  adjust sensorID (CPD.Interval _ b) =
    _2 . field @"downstreamCmds" %~
      LM.map
        ( \dc ->
          if DC.id dc == sensorID
          then dc & field @"maxValue" .~ (Operations $ floor b)
          else dc
        )
instance HasLensMap (CmdID, Cmd) ActiveSensorKey ActiveSensor where

  lenses (cmdID, cmd) =
    addPath (field @"downstreamCmds") <$> lenses (downstreamCmds cmd)
