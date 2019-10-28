{-# LANGUAGE DerivingVia #-}

-- |
-- Module      : NRM.Types.DownstreamThread
-- Copyright   : (c) UChicago Argonne, 2019
-- License     : BSD3
-- Maintainer  : fre@freux.fr
module NRM.Types.DownstreamThread
  ( DownstreamThread (..),
  )
where

import Data.Aeson
import Data.Data
import Data.JSON.Schema
import Data.MessagePack
import NRM.Classes.Messaging
import Protolude
import NRM.Types.Units

data DownstreamThread
  = DownstreamThread
      { maxValue :: Operations,
        ratelimit :: Frequency
      }
  deriving (Eq, Ord, Show, Generic, Data, MessagePack)
  deriving (JSONSchema, ToJSON, FromJSON) via GenericJSON DownstreamThread
