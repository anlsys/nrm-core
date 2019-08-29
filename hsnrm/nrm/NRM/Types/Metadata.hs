{-# LANGUAGE DerivingVia #-}

{-|
Module      : NRM.Types.Metadata
Copyright   : (c) UChicago Argonne, 2019
License     : BSD3
Maintainer  : fre@freux.fr
-}
module NRM.Types.Metadata
  ( Range (..)
  , Value (..)
  )
where

import Data.Aeson
import Data.JSON.Schema
import NRM.Classes.Messaging
import Data.MessagePack
import Protolude

newtype DValue = Value Text
  deriving (Eq, Ord, Show, Read, Generic, MessagePack)
  deriving (JSONSchema, ToJSON, FromJSON) via GenericJSON DValue

data Range = Discrete [DValue] | Interval Double Double
  deriving (Eq, Ord, Show, Read, Generic, MessagePack)
  deriving (JSONSchema, ToJSON, FromJSON) via GenericJSON Range
