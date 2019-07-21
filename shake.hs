{-# language OverloadedStrings #-}
{-# language PackageImports #-}

{-|
Module      : shake.hs
Description : dev tasks.
Copyright   : (c) Valentin Reis, 2018
License     : MIT
Maintainer  : fre@freux.fr
-}

import           Development.Shake
import           Protolude
import           Development.Shake.FilePath
import           Control.Monad
import           System.Process.Typed
import           System.Posix.Process
import qualified System.IO                     as SIO
                                                ( hSetBuffering
                                                , stdout
                                                , BufferMode(..)
                                                )
import           Options.Applicative           as OA
import           System.Directory
import "Glob"    System.FilePath.Glob
import           System.Environment


ghcidTarget :: [Text]
ghcidTarget =
  [ "--command"
  , "cabal " <> "new-repl hnrm"
  , "--restart=dhrun.cabal"
  , "--restart=default.nix"
  , "--restart=shell.nix"
  ]

runGhcid :: IO ()
runGhcid = do
  runProcess_ "rm -f .ghc.*"
  executeFile "ghcid" True (toS <$> ghcidTarget) Nothing

main :: IO ()
main = SIO.hSetBuffering SIO.stdout SIO.NoBuffering
  <> void (join (execParser (info (opts <**> helper) idm)))
 where
  opts :: Parser (IO ())
  opts = hsubparser
    (  OA.command
        "ghcid"
        (info (pure runGhcid) (progDesc "Run an argo-compatible nix-build."))
    <> OA.command "britt"
                  (info (pure runbritt) (progDesc "inplace brittany."))
    <> OA.command "cabal"
                  (info (pure cabal) (progDesc "generate cabal file."))
    <> OA.command "coverage"
                  (info (pure runcov) (progDesc "run code coverage."))
    <> OA.command "shake" (info (pure (runshake [])) (progDesc "run shake."))
    <> OA.command
         "readme"
         (info (pure (runshake ["README.md"]))
               (progDesc "run shake for README.md.")
         )
    <> OA.command
         "build"
         (info (pure (runshake ["build"]))
               (progDesc "run shake for cabal build.")
         )
    <> help "Type of operation to run."
    )


runbritt =
  mapM glob ["*.hs", "*/*.hs", "*/*/*.hs", "*/*/*.hs"] <&> concat >>= mapM_
    (\fn -> runProcess_ $ shell ("brittany --write-mode inplace " <> toS fn))

cabal = runProcess_ $ shell "dhall-to-cabal ./cabal.dh"

runcov = do
  runProcess_ "cabal clean"
  runProcess_ "cabal configure --enable-tests --enable-coverage"
  runProcess_ "cabal test"

runshake as = withArgs as $ shakeArgs shakeOptions $ phony "build" $ liftIO
  (runProcess_ $ proc "cabal" ["build"])
