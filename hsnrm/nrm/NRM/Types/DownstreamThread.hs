{-# LANGUAGE DerivingVia #-}

{-|
Module      : NRM.Types.DownstreamThread
Copyright   : (c) UChicago Argonne, 2019
License     : BSD3
Maintainer  : fre@freux.fr
-}
module NRM.Types.DownstreamThread
  ( DownstreamThread (..)
  )
where

import Data.Aeson
import Data.JSON.Schema
import Data.MessagePack
import NRM.Classes.Messaging
import Protolude

data DownstreamThread = DownstreamThread
  deriving (Eq, Ord, Show, Generic, MessagePack)
  deriving (JSONSchema, ToJSON, FromJSON) via GenericJSON DownstreamThread
