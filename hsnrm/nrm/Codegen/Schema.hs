-- |
-- Module      : Codegen.Schema
-- Copyright   : (c) 2019, UChicago Argonne, LLC.
-- License     : MIT
-- Maintainer  : fre@freux.fr
module Codegen.Schema
  ( NSchema (..),
    generatePretty,
    toAeson,
  )
where

import qualified Data.Aeson as A
import Data.Aeson.Encode.Pretty as AP
import qualified Data.Aeson.Types as AT
import Data.HashMap.Strict as H
  ( fromList,
  )
import qualified Data.JSON.Schema as S
import Data.Vector as V
  ( fromList,
  )
import NRM.Classes.Messaging as M
import Protolude

newtype NSchema = NSchema S.Schema

mkArray :: [AT.Value] -> AT.Value
mkArray = AT.Array . V.fromList

mkString :: Text -> AT.Value
mkString = AT.String

mkBool :: Bool -> AT.Value
mkBool = AT.Bool

mkSci :: Int -> AT.Value
mkSci = AT.Number . fromInteger . toInteger

toAeson :: S.Schema -> AT.Value
toAeson = AT.Object . H.fromList . toOP

toOP :: IsString a => S.Schema -> [(a, AT.Value)]
toOP (S.Choice schs) = [("oneOf", ss)] where ss = mkArray (toAeson <$> schs)
toOP (S.Object fields) =
  catMaybes
    [ Just ("type", mkString "object"),
      mqRequired objectnames,
      Just ("properties", (AT.Object . H.fromList) objectdescs)
    ]
  where
    objectnames = mkString . toS . S.key <$> filter S.required fields
    objectdescs = (\S.Field {..} -> (key, toAeson content)) <$> fields
    mqRequired :: (IsString a) => [AT.Value] -> Maybe (a, AT.Value)
    mqRequired = \case
      [] -> Nothing
      xs -> Just ("required", mkArray xs)
toOP (S.Map sch) =
  [("type", AT.String "object"), ("additionalProperties", toAeson sch)]
toOP (S.Array S.LengthBound {..} uniqueitems sch) =
  catMaybes
    [ Just ("type", mkString "array"),
      Just ("uniqueItems", mkBool uniqueitems),
      Just ("items", toAeson sch),
      ("minLength",) . mkSci <$> lowerLength,
      ("maxLength",) . mkSci <$> upperLength
    ]
toOP (S.Tuple schs) =
  [("type", mkString "array"), ("items", mkArray (toAeson <$> schs))]
toOP (S.Value S.LengthBound {..}) =
  catMaybes
    [ Just ("type", mkString "string"),
      ("minLength",) . mkSci <$> lowerLength,
      ("maxLength",) . mkSci <$> upperLength
    ]
toOP S.Boolean = [("type", mkString "boolean")]
toOP (S.Number S.Bound {..}) =
  catMaybes
    [ Just ("type", mkString "number"),
      ("minimum",) . mkSci <$> lower,
      ("maximum",) . mkSci <$> upper
    ]
toOP (S.Constant aesonValue) =
  [("const", mkString $ toS (A.encode aesonValue))]
toOP S.Any = []

generatePretty :: (M.NRMMessage a) => Proxy a -> Text
generatePretty = toS . AP.encodePretty . toAeson . M.messageSchema
