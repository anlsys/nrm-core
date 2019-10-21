{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE NoImplicitPrelude #-}
{-# LANGUAGE PackageImports #-}

{-|
Module      : shake.hs
Description : dev tasks.
Copyright   : (c) Valentin Reis, 2018
License     : MIT
Maintainer  : fre@freux.fr
-}

import Control.Monad
import Data.Text (dropEnd, strip)
import Development.Shake hiding (getEnv)
import Development.Shake.FilePath
import Options.Applicative as OA
import Protolude
import Data.Text (lines)
import System.Directory
import System.Environment (getEnv, withArgs)
import qualified Prelude
import "Glob" System.FilePath.Glob
import qualified System.IO as SIO
  ( BufferMode (..)
  , hSetBuffering
  , stdout
  )
import System.Posix.Process
import System.Process.Typed

ghcidTarget :: Text -> Text -> Maybe Text -> [Text]
ghcidTarget cabalfile target test =
  [ "-C"
  , "hsnrm"
  , "--command"
  , "cabal " <> "v2-repl " <> target <> " --ghc-option=-fno-code" <> " --builddir=../_build"
  , "--restart=hsnrm.cabal"
  , "--restart=default.nix"
  , "--restart=shell.nix"
  , "-l"
  ] ++
    toList
      ( ("--test=" <>) <$>
        test
      )

runGhcid :: Text ->Text -> Maybe Text -> IO ()
runGhcid cabalfile target test = do
  runProcess_ "rm -f .ghc.*"
  runProcess_ $ setWorkingDir "hsnrm" $ shell "cp -f $CABALFILE hsnrm.cabal"
  executeFile "ghcid" True (toS <$> ghcidTarget cabalfile target test) Nothing

main :: IO ()
main = do
  cabalFile <- toS <$> getEnv "CABALFILE"
  SIO.hSetBuffering SIO.stdout SIO.NoBuffering <>
    void (join (execParser (info (opts cabalFile <**> helper) idm)))
  where
    opts ::  Text ->Parser (IO ())
    opts cabalFile=
      hsubparser
        ( OA.command
          "ghcid"
          ( info (runGhcid cabalFile<$> targetParser <*> testParser)
            (progDesc "Run an argo-compatible nix-build.")
          ) <>
          OA.command "britt"
            (info (pure runbritt) (progDesc "inplace brittany.")) <>
          OA.command "cabal"
            (info (pure cabal) (progDesc "generate cabal file.")) <>
          OA.command "cabalstatic"
            (info (pure cabalstatic) (progDesc "generate cabal file for static build.")) <>
          OA.command "shake" (info (pure (runshake [])) (progDesc "run shake.")) <>
          OA.command
            "build"
            ( info (pure (runshake ["build"]))
              (progDesc "run shake for cabal build.")
            ) <>
          OA.command
            "codegen"
            ( info (pure (runshake ["codegen"]))
              (progDesc "run shake for cabal build.")
            ) <>
          OA.command
            "doc"
            ( info (pure (runshake ["doc"]))
              (progDesc "run shake for cabal build.")
            ) <>
          OA.command
            "client"
            ( info (pure (runshake ["client"]))
              (progDesc "run shake for cabal build.")
            ) <>
          help "Type of operation to run."
        )

targetParser :: OA.Parser Text
targetParser =
  OA.strArgument
    (OA.metavar "TARGET" <> OA.help "The ghcid target")

testParser :: OA.Parser (Maybe Text)
testParser =
  OA.optional
    ( OA.strArgument
      (OA.metavar "TEST" <> OA.help "The ghcid target")
    )

runbritt =
  mapM glob ["*.hs", "*/*.hs", "*/*/*.hs", "*/*/*.hs"] <&> concat >>=
    mapM_
      (\fn -> runProcess_ $ shell ("brittany --write-mode inplace " <> toS fn))

cabal = runProcess_ $ shell "dhall-to-cabal ./dev/pkgs/hsnrm/dev.dhall --output-dir-cwd hsnrm"

cabalstatic = runProcess_ $ shell "dhall-to-cabal ./dev/pkgs/hsnrm/static.dhall --output-dir-cwd hsnrm"

runshake as =
  withArgs as $ shakeArgs shakeOptions $ do
    phony "build" $ do
      version <- liftIO $ toS . strip . toS <$> readProcessStdout_ "ghc --numeric-version"
      ghcPathRaw <- liftIO $ strip . toS <$> readProcessStdout_ "which ghc"
      let ghcPath = dropEnd 8 ghcPathRaw
      liftIO ( runProcess_ $ setWorkingDir "hsnrm" $ shell "cp -f $CABALFILE hsnrm.cabal")
      liftIO
        ( runProcess_ $ setWorkingDir "hsnrm" $
          proc "cabal"
            [ "v2-build"
            , "nrm.so"
            , "--ghc-option=-lHSrts-ghc" <> version
            , "--ghc-option=-L" <> toS ghcPath <> "/lib/ghc-" <> version <> "/rts/"
            , "--builddir=../_build"
            , "--jobs=4"
            ]
        )
      liftIO
        ( runProcess_ $ setWorkingDir "hsnrm" $
          proc "cabal"
            [ "v2-run"
            , "--builddir=../_build"
            , "codegen", "../resources/"
            ]
        )
    phony "codegen" $
      liftIO
        ( runProcess_ $ setWorkingDir "hsnrm" $
          proc "cabal"
            [ "v2-run"
            , "--builddir=../_build"
            , "codegen"," ../resources/"
            ]
        )
    phony "client" $
      liftIO
        ( runProcess_ $ setWorkingDir "hsnrm" $
          proc "cabal"
            [ "v2-build"
            , "nrm"
            , "--builddir=../_build"
            ]
        )
    phony "doc" $do
      (exitCode, out) <- liftIO
        ( readProcessStdout $ setWorkingDir "hsnrm" $
          proc "cabal"
            [ "v2-haddock"
            , "nrm.so"
            , "--haddock-hyperlink-source"
            , "--builddir=../_build"
            ]
        )
      putText $ toS out
      let path = Prelude.last $ Data.Text.lines $ toS out
      liftIO ( runProcess_ $ proc "cp" ["-r",dropFileName $  toS path  , "html" ])
