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
  )
where

import qualified Data.Text.Prettyprint.Doc as Pretty
import qualified Data.Text.Prettyprint.Doc.Render.Text as Pretty
import qualified Dhall.Core
import Protolude
import System.FilePath
  ( dropTrailingPathSeparator,
    joinPath,
    normalise,
    splitDirectories,
    takeDirectory,
  )
import qualified System.IO

writeOutput :: Text -> FilePath -> Dhall.Core.Expr s Dhall.Core.Import -> IO ()
writeOutput header dest expr =
  System.IO.withFile dest System.IO.WriteMode $ \hnd -> do
    System.IO.hPutStrLn hnd (toS header)
    Pretty.renderIO
      hnd
      $ Pretty.layoutSmart
        prettyOpts
        (Pretty.pretty expr)
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
