{-# LANGUAGE DerivingVia #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# OPTIONS_GHC -fno-warn-orphans #-}

-- |
-- Module      : NRM.Classes.Messaging
-- Copyright   : (c) UChicago Argonne, 2019
-- License     : BSD3
-- Maintainer  : fre@freux.fr
module NRM.Classes.Messaging
  ( NRMMessage (..),
    GenericJSON (..),
    JSONSchema (..),
  )
where

import qualified Data.Aeson as A
import Data.Aeson.Encode.Pretty as AP (encodePretty)
import qualified Data.Aeson.Types as AT (parseMaybe)
import Data.JSON.Schema as S
import qualified Data.JSON.Schema.Generic as SG
import Data.JSON.Schema.Types (Schema)
import Generics.Deriving.ConNames (ConNames)
import Generics.Generic.Aeson
import qualified Generics.Generic.Aeson as AG
import Generics.Generic.IsEnum (GIsEnum)
import LMap.Map as LM
import Protolude

class
  (Generic b, SG.GJSONSchema (Rep b), AG.GfromJson (Rep b), AG.GtoJson (Rep b), GIsEnum (Rep b), ConNames (Rep b)) =>
  NRMMessage a b
    | a -> b where

  {-# MINIMAL fromJ, toJ #-}

  fromJ :: b -> a

  toJ :: a -> b

  encodePretty :: a -> ByteString
  encodePretty = toS . AP.encodePretty . AG.gtoJson . toJ

  encode :: a -> ByteString
  encode = toS . A.encode . AG.gtoJson . toJ

  decode :: ByteString -> Maybe a
  decode = fmap fromJ . AT.parseMaybe AG.gparseJson <=< A.decodeStrict

  decodeT :: Text -> Maybe a
  decodeT = fmap fromJ . AT.parseMaybe AG.gparseJson <=< A.decodeStrict . toS

  encodeT :: a -> Text
  encodeT = toS . A.encode . AG.gtoJson . toJ

  messageSchema :: Proxy a -> Schema
  messageSchema (Proxy :: Proxy a) = SG.gSchema (Proxy :: Proxy b)

newtype GenericJSON (a :: Type) = GenericJSON {unGenericJSON :: a}

instance
  ( GIsEnum (Rep a),
    ConNames (Rep a),
    GJSONSchema (Rep a),
    Generic a
  ) =>
  S.JSONSchema (GenericJSON a)
  where
  schema _ = gSchema (Proxy :: Proxy a)

instance
  ( GIsEnum (Rep a),
    ConNames (Rep a),
    GfromJson (Rep a),
    Generic a
  ) =>
  A.FromJSON (GenericJSON a)
  where
  parseJSON x = GenericJSON <$> AG.gparseJson x

instance
  ( GIsEnum (Rep a),
    ConNames (Rep a),
    GtoJson (Rep a),
    Generic a
  ) =>
  A.ToJSON (GenericJSON a)
  where
  toJSON = AG.gtoJson . unGenericJSON

deriving via GenericJSON (LM.Map a b) instance (JSONSchema a, JSONSchema b) => JSONSchema (LM.Map a b)

deriving via GenericJSON (LM.Map a b) instance (A.FromJSON a, A.FromJSON b) => A.FromJSON (LM.Map a b)

deriving via GenericJSON (LM.Map a b) instance (A.ToJSON a, A.ToJSON b) => A.ToJSON (LM.Map a b)
