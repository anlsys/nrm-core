{-|
Module      : Nrm.Configuration
Description : Nrm configuration
Copyright   : (c) UChicago Argonne, 2019
License     : BSD3
Maintainer  : fre@freux.fr
-}
module Nrm.Configuration
  ( ContainerRuntime (..)
  )
where

import Protolude
import Dhall

data Configuration
  = Configuration
      { containerRuntimeConfig :: Text
      }
