{-|
Module      : Nrm.Types.Client
Copyright   : (c) UChicago Argonne, 2019
License     : BSD3
Maintainer  : fre@freux.fr
-}
module Nrm.Types.Client
  ( ClientUUID (..)
  , nextClientUUID
  , ClientVerbosity (..)
  )
where

import Data.UUID
import Data.UUID.V1
import Protolude

newtype ClientUUID = ClientUUID UUID
  deriving (Show, Eq, Ord, Generic)

nextClientUUID :: IO (Maybe ClientUUID)
nextClientUUID = fmap ClientUUID <$> nextUUID

data ClientVerbosity = Normal | Verbose
  deriving (Eq, Show)
