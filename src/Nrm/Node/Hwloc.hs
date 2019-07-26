{-# LANGUAGE NoImplicitPrelude #-}

{-|
Module      : Nrm.Node.Hwloc
Description : Hwloc tree queries
Copyright   : (c) 2019, UChicago Argonne, LLC.
License     : BSD3
Maintainer  : fre@freux.fr
-}
module Nrm.Node.Hwloc
  ( getHwlocData
  , selectCoreIDs
  , selectPUIDs
  )
where

import Control.Arrow.ArrowTree (deep)
import Control.Arrow.ListArrow (runLA)
import Nrm.Types.Topo
import Protolude
import System.Process.Typed
import Text.XML.HXT.Arrow.XmlArrow
import Text.XML.HXT.DOM.TypeDefs (XmlTrees)
import Text.XML.HXT.Parser.XmlParsec
import Text.XML.HXT.XPath.XPathEval

type HwlocData = XmlTrees

selectCoreIDs :: HwlocData -> [CoreId]
selectCoreIDs = extractOSindexes (Proxy :: Proxy CoreId)

selectPUIDs :: HwlocData -> [PUId]
selectPUIDs = extractOSindexes (Proxy :: Proxy PUId)

getHwlocData :: IO HwlocData
getHwlocData =
  readProcessStdout_ "hwloc-ls -p --whole-system --of xml" <&> xreadDoc . toS

extractOSindexes
  :: (ToHwlocType a, IdFromString a) => Proxy a -> HwlocData -> [a]
extractOSindexes typeAttr xml =
  catMaybes $
    idFromString <$>
    concat
      ( runLA (deep (getAttrValue "os_index")) <$>
        selectSubtreesOfType (getType typeAttr) xml
      )

selectSubtreesOfType :: Text -> HwlocData -> XmlTrees
selectSubtreesOfType typeAttr hwld =
  concat $ getXPath (".//*[@type='" <> toS typeAttr <> "']") <$> hwld
