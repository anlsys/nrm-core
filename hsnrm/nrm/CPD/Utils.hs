{-# LANGUAGE DerivingVia #-}

{-|
Module      : CPD.Utils
Copyright   : (c) UChicago Argonne, 2019
License     : BSD3
Maintainer  : fre@freux.fr
-}
module CPD.Utils
  ( validate
  )
where

import CPD.Core
import Protolude

validate :: Range -> Value -> Maybe Value
validate (Set ds) (DiscreteValue d) =
  if d `elem` ds
  then Just $ DiscreteValue d
  else Nothing
validate (Interval a b) (ContinuousValue d) =
  if a <= d && d <= b
  then Just $ ContinuousValue d
  else Nothing
validate _ _ = Nothing
