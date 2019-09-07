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

data Validation = Adjust Range | Ok | Illegal

validate :: Range -> Value -> Validation
validate (Set ds) (DiscreteValue d) =
  if d `elem` ds
  then Ok
  else Illegal
validate (Interval a b) (ContinuousValue d)
  | a <= d && d <= b = Ok
  | d < a = Adjust $ Interval (2 * a - b) b
  | otherwise = Adjust $ Interval a (2 * b - a)
validate _ _ = Illegal
