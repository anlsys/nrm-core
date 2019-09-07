{-|
Module      : NRM.Slices.Singularity
Copyright   : (c) 2019, UChicago Argonne, LLC.
License     : BSD3
Maintainer  : fre@freux.fr

This module offers an interface to singularity slices.
-}
module NRM.Slices.Singularity
  ( SingularityRuntime (..)
  )
where

import Data.MessagePack
import Data.Aeson
import Data.Data
import Protolude

data SingularityRuntime = SingularityRuntime
  deriving (Show, Generic,Data, MessagePack, ToJSON, FromJSON)
