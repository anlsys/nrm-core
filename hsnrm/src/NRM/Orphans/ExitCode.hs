{-# LANGUAGE DerivingVia #-}
{-# OPTIONS_GHC -fno-warn-orphans #-}

-- |
-- Module      : NRM.Orphans.ExitCode
-- Copyright   : (c) UChicago Argonne, 2019
-- License     : BSD3
-- Maintainer  : swann@anl.gov
module NRM.Orphans.ExitCode
  (
  )
where

import Data.Aeson
import Data.Data
import Data.JSON.Schema
import Data.MessagePack
import NRM.Classes.Messaging
import Protolude

instance MessagePack ExitCode where

  toObject ExitSuccess = toObject (0 :: Int)
  toObject (ExitFailure i) = toObject i

  fromObject x = fromObject x <&> \y -> if y == 0 then ExitSuccess else ExitFailure y

deriving via GenericJSON ExitCode instance JSONSchema ExitCode

deriving via GenericJSON ExitCode instance ToJSON ExitCode

deriving via GenericJSON ExitCode instance FromJSON ExitCode

deriving instance Data ExitCode
