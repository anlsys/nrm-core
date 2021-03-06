{-# LANGUAGE DerivingVia #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# OPTIONS_GHC -fno-warn-orphans #-}

-- |
-- Module      : NRM.Classes.Messaging
-- Copyright   : (c) UChicago Argonne, 2019
-- License     : BSD3
-- Maintainer  : swann@anl.gov
module NRM.Classes.Messaging
  ( NRMMessage (..),
    GenericJSON (..),
    AnyJSON (..),
    JSONSchema (..),
  )
where

import qualified Data.Aeson as A
import Data.Aeson.Encode.Pretty as AP (encodePretty)
import qualified Data.Aeson.Types as AT (parseMaybe)
import Data.JSON.Schema as S
import qualified Data.JSON.Schema.Generic as SG
import Data.JSON.Schema.Types (Schema)
import Data.String
import Generics.Deriving.ConNames (ConNames)
import Generics.Generic.Aeson
import qualified Generics.Generic.Aeson as AG
import Generics.Generic.IsEnum (GIsEnum)
import Protolude

class (Generic a, SG.GJSONSchema (Rep a), AG.GfromJson (Rep a), AG.GtoJson (Rep a), GIsEnum (Rep a), ConNames (Rep a)) => NRMMessage a where

  encodePretty :: a -> ByteString
  encodePretty = toS . AP.encodePretty . AG.gtoJson

  encode :: a -> ByteString
  encode = toS . A.encode . AG.gtoJson

  decode :: ByteString -> Maybe a
  decode = AT.parseMaybe AG.gparseJson <=< A.decodeStrict

  decodeT :: Text -> Maybe a
  decodeT = AT.parseMaybe AG.gparseJson <=< A.decodeStrict . toS

  encodeT :: a -> Text
  encodeT = toS . A.encode . AG.gtoJson

  messageSchema :: Proxy a -> Schema
  messageSchema = SG.gSchema

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

instance
  ( A.FromJSON a,
    A.ToJSON a,
    GIsEnum (Rep a),
    ConNames (Rep a),
    GfromJson (Rep a),
    Generic a
  ) =>
  IsString (GenericJSON a)
  where
  fromString s =
    A.decode (toS s) & \case
      Nothing -> ""
      Just x -> x

newtype AnyJSON (a :: Type) = AnyJSON {unAnyJSON :: a}

instance S.JSONSchema (AnyJSON a) where
  schema _ = S.Any
