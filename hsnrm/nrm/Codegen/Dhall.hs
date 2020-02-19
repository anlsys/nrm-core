{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE ViewPatterns #-}

-- |
-- Module      : Codegen.Dhall
-- Copyright   : Copyright (c) 2018 Oliver Charles.
-- License     : MIT License.
--
-- Dhall code generation utilities adapted from dhall-to-cabal-meta.
module Codegen.Dhall
  ( writeOutput,
    relativeTo,
    takeDirectory,
    typeToExpr,
    defaultToExpr,
    valueToExpr,
    exprToText,
  )
where

import Data.Default
import qualified Data.Text.Prettyprint.Doc as Pretty
import qualified Data.Text.Prettyprint.Doc.Render.Text as Pretty
import Dhall
import Dhall.Core as Dhall
import Dhall.Parser as Dhall
import Dhall.Pretty as Dhall
import Dhall.TypeCheck as Dhall
import Protolude
import System.FilePath
  ( dropTrailingPathSeparator,
    joinPath,
    normalise,
    splitDirectories,
    takeDirectory,
  )
import qualified System.IO

writeOutput :: (Pretty.Pretty a) => Text -> FilePath -> Expr s a -> IO ()
writeOutput header dest e =
  System.IO.withFile dest System.IO.WriteMode $ \hnd -> do
    System.IO.hPutStrLn hnd (toS header)
    Pretty.renderIO
      hnd
      $ Pretty.layoutSmart
        prettyOpts
        (Pretty.pretty e)
    System.IO.hPutStr hnd "\n"

prettyOpts :: Pretty.LayoutOptions
prettyOpts =
  Pretty.defaultLayoutOptions
    { Pretty.layoutPageWidth = Pretty.AvailablePerLine 80 1.0
    }

relativeTo ::
  -- | The path to be relative to. Note that the final file-name is
  -- ignored: @foo/bar@ is relative to @foo/@, even if @foo/bar@ is
  -- a directory.
  FilePath ->
  -- | The path to relativise.
  FilePath ->
  FilePath
relativeTo =
  \(splitDirectories . dropTrailingPathSeparator . takeDirectory . normalise -> base) ->
    \(splitDirectories . normalise -> path) ->
      joinPath (go base path)
  where
    -- @normalise "."@ is @"."@, so we have to take care here with dots.
    go :: [FilePath] -> [FilePath] -> [FilePath]
    go (a : as) (b : bs)
      | a == b = go as bs
      | a == "." = go as (b : bs)
      | b == "." = go (a : as) bs
      | otherwise = (".." <$ (a : as)) ++ (b : bs)
    go [] bs = bs
    go as [] = ".." <$ as

typeToExpr :: Interpret x => Proxy x -> Expr Src b
typeToExpr (Proxy :: Proxy x) =
  Dhall.absurd <$> Dhall.expected (Dhall.auto :: Dhall.Type x)

defaultToExpr :: (Inject x, Default x) => Proxy x -> Expr Src b
defaultToExpr (Proxy :: Proxy x) =
  Dhall.absurd
    <$> embed
      (injectWith defaultInterpretOptions)
      (def :: x)

valueToExpr :: (Inject x) => x -> Expr Src X
valueToExpr x =
  Dhall.absurd
    <$> embed
      (injectWith defaultInterpretOptions)
      x

exprToText :: (Pretty.Pretty a) => Expr Src a -> Text
exprToText = show . Dhall.prettyExpr
