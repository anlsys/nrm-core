{-|
Module      : Nrm.Types.Metadata
Copyright   : (c) UChicago Argonne, 2019
License     : BSD3
Maintainer  : fre@freux.fr
-}
module Nrm.Types.Metadata
  ( Range (..)
  , Value (..)
  )
where

import Data.MessagePack
import Protolude

data Value = Value Text
  deriving (Eq, Ord, Show, Read, Generic)

data Range = Discrete [Value] | Interval Double Double
  deriving (Eq, Ord, Show, Read, Generic)

deriving instance MessagePack Range
deriving instance MessagePack Value
