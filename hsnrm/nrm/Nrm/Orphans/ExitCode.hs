{-# OPTIONS_GHC -fno-warn-orphans #-}
{-|
Module      : Nrm.Orphans.ExitCode
Copyright   : (c) UChicago Argonne, 2019
License     : BSD3
Maintainer  : fre@freux.fr
-}
module Nrm.Orphans.ExitCode
  (
  )
where

import Data.Aeson
import Data.JSON.Schema
import Data.MessagePack
import Generics.Generic.Aeson
import Protolude

instance MessagePack ExitCode where

  toObject ExitSuccess = toObject (0 :: Int)
  toObject (ExitFailure i) = toObject i

  fromObject x = fromObject x <&> \y -> if y == 0 then ExitSuccess else ExitFailure y

instance ToJSON ExitCode where

  toJSON = gtoJson

instance FromJSON ExitCode where

  parseJSON = gparseJson

instance JSONSchema ExitCode where

  schema = gSchema
