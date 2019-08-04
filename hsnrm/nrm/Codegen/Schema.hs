{-|
Module      : Codegen.Schema
Description : types
Copyright   : (c) 2019, UChicago Argonne, LLC.
License     : MIT
Maintainer  : fre@freux.fr
-}
module Codegen.Schema
  ( upstreamReqSchema
  , {-, upstreamRepSchema-}
    {-, upstreamPubSchema-}
    {-, downstreamEventSchema-}
    libnrmHeader
  , main
  , NSchema (..)
  )
where

import Codegen.CHeader
import qualified Data.Aeson as A
import qualified Data.Aeson.Types as AT
import Data.Map as H (fromList)
import qualified Data.JSON.Schema as S
import Data.Vector as V
  ( fromList
  )
import Protolude

--  data Schema =
--      Choice [Schema]      -- ^ A choice of multiple values, e.g. for sum types.
--    | Object [Field]       -- ^ A JSON object.
--    | Map    Schema        -- ^ A JSON object with arbitrary keys.
--    | Array LengthBound Bool Schema
--                           -- ^ An array. The LengthBound represent the
--                           -- lower and upper bound of the array
--                           -- size. The value 'unboundedLength' indicates no bound.
--                           -- The boolean denotes whether items have
--                           -- to be unique.
--    | Tuple [Schema]       -- ^ A fixed-length tuple of different values.
--    | Value LengthBound    -- ^ A string. The LengthBound denote the lower and
--                           -- upper bound of the length of the string. The
--                           -- value 'unboundedLength' indicates no bound.
--    | Boolean              -- ^ A Bool.
--    | Number Bound         -- ^ A number. The Bound denote the lower and
--                           -- upper bound on the value. The value 'unbounded'
--                           -- indicates no bound.
--    | Constant Aeson.Value -- ^ A Value that never changes. Can be
--                           -- combined with Choice to create enumerables.
--    | Any                  -- ^ Any value is allowed.
--    deriving (Eq, Show)

--data Field = Field { key :: Text, required :: Bool, content :: Schema } deriving (Eq, Show)
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
    [ Just ("type", mkString "object")
    , mqRequired objectnames
    , Just ("properties", (AT.Object . H.fromList) objectdescs)
    ]
  where
    objectnames = mkString . toS . S.key <$> filter S.required fields
    objectdescs = (\S.Field {..} -> (key, toAeson content)) <$> fields
    mqRequired = \case
      [] -> Nothing
      xs -> Just ("required", mkArray xs)
toOP (S.Map sch) =
  [("type", AT.String "object"), ("additionalProperties", toAeson sch)]
toOP (S.Array S.LengthBound {..} uniqueitems sch) =
  catMaybes
    [ Just ("type", mkString "array")
    , Just ("uniqueItems", mkBool uniqueitems)
    , Just ("items", toAeson sch)
    , ("minLength",) . mkSci <$> lowerLength
    , ("maxLength",) . mkSci <$> upperLength
    ]
toOP (S.Tuple schs) =
  [("type", mkString "array"), ("items", mkArray (toAeson <$> schs))]
toOP (S.Value S.LengthBound {..}) =
  catMaybes
    [ Just ("type", mkString "string")
    , ("minLength",) . mkSci <$> lowerLength
    , ("maxLength",) . mkSci <$> upperLength
    ]
toOP S.Boolean = [("type", mkString "boolean")]
toOP (S.Number S.Bound {..}) =
  catMaybes
    [ Just ("type", mkString "number")
    , ("minimum",) . mkSci <$> lower
    , ("maximum",) . mkSci <$> upper
    ]
toOP (S.Constant aesonValue) =
  [("const", mkString $ toS (A.encode aesonValue))]
toOP S.Any = []

{-generatePretty :: (S.JSONSchema a) => Proxy a -> Text-}
{-generatePretty = toS . A.encode . toAeson . S.schema-}

{-upstreamReqSchema :: Text-}
{-upstreamReqSchema = generatePretty (Proxy :: Proxy Req.S)-}

{-upstreamRepSchema :: Text-}
{-upstreamRepSchema = generatePretty (Proxy :: Proxy Rep.S)-}

{-upstreamPubSchema :: Text-}
{-upstreamPubSchema = generatePretty (Proxy :: Proxy Pub.S)-}

{-downstreamEventSchema :: Text-}
{-downstreamEventSchema = generatePretty (Proxy :: Proxy Event.S)-}

{-libnrmHeader :: Text-}
{-libnrmHeader = toHeader $ toCHeader (Proxy :: Proxy Event.S)-}
