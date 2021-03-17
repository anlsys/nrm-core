{-# LANGUAGE ScopedTypeVariables #-}

-- |
-- Module      : Codegen.CHeader
-- Copyright   : (c) 2019, UChicago Argonne, LLC.
-- License     : MIT
-- Maintainer  : fre@freux.fr
module Codegen.CHeader
  ( toCHeader,
  )
where

{-import Data.JSON.Schema.Generator as G-}

import Data.Aeson as A
import Data.Aeson.Types as AT
import qualified Data.HashMap.Strict as H
import qualified Data.JSON.Schema as S
import Data.Text (replace, toUpper)
import qualified Data.Vector as V (fromList)
import qualified NRM.Classes.Messaging as M
import Protolude hiding (Any)

toCHeader :: (M.NRMMessage a) => Proxy a -> Text
toCHeader = rep . toHeader . goToplevel . M.messageSchema
  where
    rep = Data.Text.replace "\\\"%d\\\"" "%d"

toHeader :: [(Text, AT.Value)] -> Text
toHeader h =
  mconcat . intersperse "\n" $
    h <&> \(msgname, khl) ->
      "#define NRM_"
        <> toUpper msgname
        <> "_FORMAT "
        <> show (A.encode (AT.Array $ V.fromList
                   [AT.Object $ H.singleton "timestamp" "%lld",
                    AT.Object $ H.singleton msgname khl]))

goToplevel :: S.Schema -> [(Text, AT.Value)]
goToplevel (S.Choice schemas) = mconcat $ goToplevel <$> schemas
goToplevel (S.Object [(S.Field key _ content)]) = [(key, go content)]
goToplevel (S.Tuple [_timestamp, info]) = goToplevel info
goToplevel _ = panic "schema first level malformed"

go :: S.Schema -> AT.Value
go (S.Tuple schemas) = AT.Array (V.fromList $ go <$> schemas)
go (S.Object fields) = AT.Object . H.fromList $
  fields <&> \(S.Field key _ content) -> (key, go content)
go (S.Value _) = "%s"
go (S.Number _) = AT.String "%d"
go _ = panic "error: unallowed in message format"
