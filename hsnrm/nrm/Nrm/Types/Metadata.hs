{-|
Module      : Nrm.Types.Metadata
Copyright   : (c) UChicago Argonne, 2019
License     : BSD3
Maintainer  : fre@freux.fr
-}
module Nrm.Types.Metadata
  ( Range (..)
  )
where

data Range a = Discrete [a] | Interval a a
