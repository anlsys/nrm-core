{-# language NoImplicitPrelude #-}
module 
  ( main
  )
where
import           Protolude
import           Prelude                        ( String )
import           System.Process.Typed
import           Text.XML.HXT.Parser.XmlParsec
import           Text.XML.HXT.XPath.XPathEval
import           Text.XML.HXT.DOM.TypeDefs      ( XmlTrees )
import           Text.Pretty.Simple
import           Control.Arrow.ArrowTree        ( deep )

import           Text.XML.HXT.Arrow.XmlArrow
import           Control.Arrow.ListArrow        ( runLA )

type HwlocData = XmlTrees
newtype CoreId = CoreId Integer deriving (Show)
newtype PUId = PUId Integer deriving (Show)
newtype PkgId = PkgId Integer deriving Read

class IdFromString a where
  idFromString :: String -> Maybe a
instance IdFromString CoreId where
  idFromString s = CoreId <$> readMaybe s
instance IdFromString PUId where
  idFromString s = PUId <$> readMaybe s

class ToHwlocType a where
  getType :: Proxy a -> Text
instance ToHwlocType PUId where
  getType _ = "PU"
instance ToHwlocType CoreId where
  getType _ = "Core"

main :: IO ()
main = do
  hwlocData <- getHwlocData
  pPrint $ extractOSindexes (Proxy :: Proxy PUId) hwlocData
  pPrint $ extractOSindexes (Proxy :: Proxy CoreId) hwlocData

getHwlocData :: IO HwlocData
getHwlocData =
  readProcessStdout_ "hwloc-ls -p --whole-system --of xml" <&> xreadDoc . toS

extractOSindexes
  :: (ToHwlocType a, IdFromString a) => Proxy a -> HwlocData -> [a]
extractOSindexes typeAttr xml = catMaybes
  (idFromString <$> concat
    (   runLA (deep (getAttrValue "os_index"))
    <$> selectSubtreesOfType (getType typeAttr) xml
    )
  )

selectSubtreesOfType :: Text -> HwlocData -> XmlTrees
selectSubtreesOfType typeAttr hwld =
  concat $ getXPath (".//*[@type='" <> toS typeAttr <> "']") <$> hwld
