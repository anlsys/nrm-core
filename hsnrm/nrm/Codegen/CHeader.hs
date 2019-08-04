{-# language ScopedTypeVariables #-}

{-|
Module      : Codegen.CHeader
Description : types
Copyright   : (c) 2019, UChicago Argonne, LLC.
License     : MIT
Maintainer  : fre@freux.fr
-}
module Codegen.CHeader
  ( CHeader
  , toHeader
  , CHeaderGen (toCHeader)
  , GCHeaderGen (gToCHeader)
  , genericToCHeader
  )
where

{-import Data.JSON.Schema.Generator as G-}
import qualified Data.Text as T
import GHC.Generics
  ( C
  , S
  )
import Protolude
import qualified Prelude
  ( undefined
  )

type MessageName = Text

type Key = Text

data HeaderType = HDouble | HInt | HString

toHeaderText :: HeaderType -> Text
toHeaderText HDouble = " \\\"%f\\\""
toHeaderText HInt = " \\\"%d\\\""
toHeaderText HString = " \\\"%s\\\""

type CHeaderEntries = [(Key, HeaderType)]

type CHeader = [(MessageName, CHeaderEntries)]

toHeader :: CHeader -> Text
toHeader h =
  mconcat $ intersperse "\n" $ h <&> \(msgname, khl) ->
    "#define NRM_" <>
      T.toUpper msgname <>
      "_FORMAT \"{\\\"tag\\\":\\\"" <>
      T.toLower msgname <>
      "\\\"," <>
      mconcat (intersperse "," (toField <$> khl)) <>
      "}\""
  where
    toField :: (Key, HeaderType) -> Text
    toField (key, tpe) = key <> ":" <> toHeaderText tpe

class CHeaderGen a where

  toCHeader :: Proxy a -> CHeader

  default toCHeader
    :: (Generic a, GCHeaderGen (Rep a))
    => Proxy a
    -> CHeader
  toCHeader = genericToCHeader

class GCHeaderGen f where

  gToCHeader :: Proxy (f a) -> CHeader

genericToCHeader
  :: (Generic a, GCHeaderGen (Rep a))
  => Proxy a
  -> CHeader
genericToCHeader = gToCHeader . fmap from

class GCHeaderGenEntries f where

  gToCHeaderEntries :: Proxy (f a) -> CHeaderEntries

class CHeaderType a where

  headerType :: Proxy a -> HeaderType

instance
  (GCHeaderGen a, GCHeaderGen b)
  => GCHeaderGen (a :+: b) where

  gToCHeader Proxy = gToCHeader x <> gToCHeader y
    where
      x = Proxy :: Proxy (a p)
      y = Proxy :: Proxy (b p)

instance
  (GCHeaderGenEntries a, GCHeaderGenEntries b)
  => GCHeaderGenEntries (a :*: b) where

  gToCHeaderEntries _ = gToCHeaderEntries a <> gToCHeaderEntries b
    where
      a = Proxy :: Proxy (a p)
      b = Proxy :: Proxy (b p)

instance
  (GCHeaderGen a)
  => GCHeaderGen (D1 meta a) where

  gToCHeader _ = gToCHeader b
    where
      b = Proxy :: Proxy (a p)

instance
  (Selector s, CHeaderType t)
  => GCHeaderGenEntries (M1 S s (Rec0 t)) where

  gToCHeaderEntries _ =
    [ ( "\\\"" <> toS selname <>
          "\\\""
      , headerType (Proxy :: Proxy t)
      )
    ]
    where
      selector = Prelude.undefined :: S1 s a p
      selname = selName selector

instance
  (Constructor c, GCHeaderGenEntries p)
  => GCHeaderGen (M1 C c p) where

  gToCHeader _ =
    [ ( toS conname
      , gToCHeaderEntries (Proxy :: Proxy (p x))
      )
    ]
    where
      conname = conName (Prelude.undefined :: (M1 C c p) x)

instance CHeaderType Text where

  headerType _ = HString

instance CHeaderType Int where

  headerType _ = HInt

instance CHeaderType Double where

  headerType _ = HDouble
