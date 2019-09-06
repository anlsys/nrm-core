{-# LANGUAGE DerivingVia #-}

{-|
Module      : NRM.Types.Process
Copyright   : (c) UChicago Argonne, 2019
License     : BSD3
Maintainer  : fre@freux.fr
-}
module NRM.Types.Process
  ( ProcessID (..)
  , ProcessState (..)
  , blankState
  , isDone
  )
where

import Data.Aeson as A
import Data.JSON.Schema
import Data.MessagePack
import Dhall
import NRM.Classes.Messaging
import NRM.Orphans.ExitCode ()
import NRM.Orphans.UUID ()
import Protolude
import qualified System.Posix.Types as P

data ProcessState
  = ProcessState
      { ended :: Maybe ExitCode
      , stdoutFinished :: Bool
      , stderrFinished :: Bool
      }
  deriving (Show, Generic, MessagePack)
  deriving (JSONSchema, ToJSON, FromJSON) via GenericJSON ProcessState

blankState :: ProcessState
blankState = ProcessState Nothing False False

isDone :: ProcessState -> Maybe ExitCode
isDone ProcessState {..} = case ended of
  Just exc | stdoutFinished && stderrFinished -> Just exc
  _ -> Nothing

newtype ProcessID = ProcessID P.CPid
  deriving (Eq, Ord, Show, Read, Generic)

instance MessagePack ProcessID where

  toObject (ProcessID x) = toObject (fromIntegral x :: Int)

  fromObject x = ProcessID . P.CPid <$> fromObject x

instance ToJSON ProcessID where

  toJSON (ProcessID x) = toJSON (fromIntegral x :: Int)

instance FromJSON ProcessID where

  parseJSON = fmap (ProcessID . P.CPid) . parseJSON

instance JSONSchema ProcessID where

  schema Proxy = schema (Proxy :: Proxy Int)
