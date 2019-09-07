{-|
Module      : NRM.Slices.Nodeos
Copyright   : (c) 2019, UChicago Argonne, LLC.
License     : BSD3
Maintainer  : fre@freux.fr

This module offers an interface to NodeOS slices.
-}
module NRM.Slices.Nodeos
  ( NodeosRuntime (..)
  )
where

import Data.Aeson
import Data.Data
import Data.MessagePack
import Protolude

data NodeosRuntime = NodeosRuntime
  deriving (Show, Generic, Data, MessagePack, ToJSON, FromJSON)
