{-|
Module      : Nrm.Containers.Singularity
Copyright   : (c) 2019, UChicago Argonne, LLC.
License     : BSD3
Maintainer  : fre@freux.fr

This module offers an interface to singularity containers.
-}
module Nrm.Containers.Singularity
  ( SingularityRuntime (..)
  )
where

import Data.MessagePack
import Data.Aeson
import Protolude

data SingularityRuntime = SingularityRuntime
  deriving (Show, Generic, MessagePack, ToJSON, FromJSON)
