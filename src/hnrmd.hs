{-# language NoImplicitPrelude #-}
module Main
  ( main
  )
where
import           Protolude
import           System.Process.Typed
import           Text.XML.HXT.Parser.XmlParsec
import           Text.XML.HXT.XPath.XPathEval
import           Text.XML.HXT.DOM.TypeDefs      ( XmlTrees )
import           Text.Pretty.Simple

type HwlocData = XmlTrees

main :: IO ()
main = do
  hwlocData <- getHwlocData
  mapM_ pPrint (getPUs hwlocData)

getHwlocData :: IO HwlocData
getHwlocData =
  readProcessStdout_ "hwloc-ls -p --whole-system --of xml" <&> xreadDoc . toS

getPUs :: HwlocData -> XmlTrees
getPUs hwld = concat $ getXPath ".//*[@type='PU']" <$> hwld
