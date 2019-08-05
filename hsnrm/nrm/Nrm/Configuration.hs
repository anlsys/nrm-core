{-|
Module      : Nrm.Configuration
Description : Nrm configuration
Copyright   : (c) UChicago Argonne, 2019
License     : BSD3
Maintainer  : fre@freux.fr
-}
module Nrm.Configuration
  ( Configuration (..)
  )
where

import Dhall
{-import Protolude-}

data Configuration
  = Configuration
      { containerRuntimeConfig :: Text
      }
  deriving (Generic, Interpret)
