{-# LANGUAGE DerivingVia #-}

{-|
Module      : NRM.Types.CPDL
Copyright   : (c) UChicago Argonne, 2019
License     : BSD3
Maintainer  : fre@freux.fr
-}
module NRM.Types.CPDL
  ( Range (..)
  , Discrete (..)
  , Value (..)
  , validate
  )
where

import qualified Data.Aeson as A
import Data.JSON.Schema
import Data.MessagePack
import NRM.Classes.Messaging
import Protolude

newtype Discrete = D Text
  deriving (Eq, Ord, Show, Read, Generic, MessagePack)
  deriving (JSONSchema, A.ToJSON, A.FromJSON) via GenericJSON Discrete

data Range
  = Set [Discrete]
  | Interval Double Double
  deriving (Eq, Ord, Show, Read, Generic, MessagePack)
  deriving (JSONSchema, A.ToJSON, A.FromJSON) via GenericJSON Range

data Value
  = DiscreteValue Discrete
  | ContinuousValue Double

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
