{-# LANGUAGE ScopedTypeVariables #-}

{-|
Module      : Nrm.Classes.Messaging
Copyright   : (c) UChicago Argonne, 2019
License     : BSD3
Maintainer  : fre@freux.fr
-}
module Nrm.Classes.Messaging
  ( NrmMessage (..)
  , JSONLayer (..)
  )
where

import qualified Data.Aeson as A
import qualified Data.JSON.Schema as S
import Data.JSON.Schema
import Protolude

class (A.FromJSON b, A.ToJSON b, S.JSONSchema b) => JSONLayer a b | a -> b where

  fromJ :: b -> a

  toJ :: a -> b

class NrmMessage a where

  encode :: a -> ByteString

  decode :: ByteString -> Maybe a

  schema :: Proxy a -> Schema

instance (JSONLayer a b) => NrmMessage a where

  decode = fmap fromJ . A.decode . toS

  encode = toS . A.encode . toJ

  schema (Proxy :: Proxy a) = S.schema (Proxy :: Proxy b)
