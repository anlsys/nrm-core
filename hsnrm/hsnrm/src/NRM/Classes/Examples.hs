-- |
-- Module      : NRM.Classes.Examples
-- Copyright   : (c) UChicago Argonne, 2019
-- License     : BSD3
-- Maintainer  : fre@freux.fr
module NRM.Classes.Examples
  ( Examples (..),
  )
where

import Protolude

class Examples a where
  examples :: Map Text a
