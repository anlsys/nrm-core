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
  , parseApplicationUUID
  , toText
  )
where

import Data.MessagePack
import qualified Data.UUID as U (UUID, fromText, toText)
import Data.UUID.V1
import Protolude

newtype ApplicationUUID = ApplicationUUID U.UUID
  deriving (Eq, Ord)

newtype Arg = Arg Text
  deriving (Generic)

deriving instance MessagePack Arg

newtype Command = Command Text
  deriving (Generic)

deriving instance MessagePack Command

newtype Arguments = Arguments [Arg]
  deriving (Generic)

deriving instance MessagePack Arguments

data AppStartConfig
  = AppStartConfig
      { command :: Command
      , arguments :: Arguments
      , applicationUUID :: ApplicationUUID
      }

nextApplicationUUID :: IO (Maybe ApplicationUUID)
nextApplicationUUID = fmap ApplicationUUID <$> nextUUID

parseApplicationUUID :: Text -> Maybe ApplicationUUID
parseApplicationUUID = fmap ApplicationUUID <$> U.fromText

toText :: ApplicationUUID -> Text
toText (ApplicationUUID u) = U.toText u
