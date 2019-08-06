{-|
Module      : Nrm.Types.Application
Copyright   : (c) UChicago Argonne, 2019
License     : BSD3
Maintainer  : fre@freux.fr
-}
module Nrm.Types.Application
  ( ApplicationUUID (..)
  , Arg (..)
  , Command (..)
  , Arguments (..)
  , AppStartConfig (..)
  , nextApplicationUUID
  )
where

import Data.UUID
import Data.UUID.V1
import Protolude

newtype ApplicationUUID = ApplicationUUID UUID
  deriving (Eq, Ord)

newtype Arg = Arg Text

newtype Command = Command Text

newtype Arguments = Arguments [Arg]

data AppStartConfig
  = AppStartConfig
      { command :: Command
      , arguments :: Arguments
      , applicationUUID :: ApplicationUUID
      }

nextApplicationUUID :: IO (Maybe ApplicationUUID)
nextApplicationUUID = fmap ApplicationUUID <$> nextUUID
