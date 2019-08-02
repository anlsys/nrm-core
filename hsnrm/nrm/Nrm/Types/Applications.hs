{-|
Module      : Nrm.Types.Applications
Description : Applications
Copyright   : (c) UChicago Argonne, 2019
License     : BSD3
Maintainer  : fre@freux.fr
-}
module Nrm.Types.Applications
  ( ApplicationUUID (..)
  , Arg (..)
  , Command (..)
  , Arguments (..)
  , StartData (..)
  )
where

import Data.UUID
import Protolude

newtype ApplicationUUID = ApplicationUUID UUID

newtype Arg = Arg Text

newtype Command = Command Text

data Arguments = Arguments [Arg]

data StartData
  = StartData
      { command :: Command
      , argument :: Arguments
      , uuid :: ApplicationUUID
      }
