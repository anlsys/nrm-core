{-# LANGUAGE ScopedTypeVariables #-}

-- |
-- Module      : NRM.Node.Hwloc
-- Description : Hwloc tree queries
-- Copyright   : (c) 2019, UChicago Argonne, LLC.
-- License     : BSD3
-- Maintainer  : fre@freux.fr
--
-- This module offers a minimalistic interface to @hwloc@.
module NRM.Node.Hwloc
  ( -- * Retrieving Hwloc XML data
    HwlocData (..),
    getHwlocData,

    -- * Running queries
    selectCoreIDs,
    selectPUIDs,
    selectPackageIDs,
  )
where

import Control.Arrow.ArrowTree (deep)
import Control.Arrow.ListArrow (runLA)
import Data.Coerce
import NRM.Classes.Topology
import NRM.Types.Topology
import Protolude
import System.Process.Typed
import Text.XML.HXT.Arrow.XmlArrow
import Text.XML.HXT.DOM.TypeDefs (XmlTrees)
import Text.XML.HXT.Parser.XmlParsec
import Text.XML.HXT.XPath.XPathEval

-- | Full Hwloc data
newtype HwlocData = HwlocData XmlTrees

-- | Lists all Core IDs from Hwloc topology information.
selectCoreIDs :: HwlocData -> [CoreID]
selectCoreIDs = extractOSindexes (Proxy :: Proxy CoreID)

-- | Lists all Processing Unit IDs from Hwloc topology information.
selectPUIDs :: HwlocData -> [PUID]
selectPUIDs = extractOSindexes (Proxy :: Proxy PUID)

-- | Lists all Package IDs from Hwloc topology information.
selectPackageIDs :: HwlocData -> [PackageID]
selectPackageIDs = extractOSindexes (Proxy :: Proxy PackageID)

-- | Runs the @hwloc@ binary in @$PATH@ to retrieve XML topology information.
getHwlocData :: IO HwlocData
getHwlocData =
  HwlocData <$> (readProcessStdout_ "hwloc-ls -p --whole-system --of xml" <&> xreadDoc . toS)

extractOSindexes :: forall a. (ToHwlocType a, Coercible a Int) => Proxy a -> HwlocData -> [a]
extractOSindexes typeAttr xml =
  catMaybes $
    fmap (coerce :: Int -> a) . readMaybe
      <$> concat
        ( runLA (deep (getAttrValue "os_index"))
            <$> selectSubtreesOfType (getType typeAttr) xml
        )

selectSubtreesOfType :: Text -> HwlocData -> XmlTrees
selectSubtreesOfType typeAttr (HwlocData hwld) =
  concat $ getXPath (".//*[@type='" <> toS typeAttr <> "']") <$> hwld
