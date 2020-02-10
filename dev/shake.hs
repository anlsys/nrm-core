{-# LANGUAGE NoImplicitPrelude #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE PackageImports #-}

import Control.Monad
import Data.Text (dropEnd, lines, splitOn, strip)
import Development.Shake hiding (getEnv)
import Development.Shake.FilePath
import Options.Applicative as OA
import Protolude
import System.Directory
import System.Environment (getEnv, withArgs)
import System.FilePath.Glob
import qualified System.IO as SIO
  ( BufferMode (..),
    hSetBuffering,
    stdout,
  )
import System.Posix.Process
import System.Process.Typed
import qualified Prelude

ghcidTarget :: Text -> Text -> Maybe Text -> [Text]
ghcidTarget cabalfile target test =
  [ "-C",
    "hsnrm",
    "--command",
    "cabal " <> "v2-repl " <> target <> " --ghc-option=-fno-code" <> " --builddir=../.build",
    "--restart=hsnrm.cabal",
    "--restart=default.nix",
    "--restart=shell.nix",
    "-l"
  ]
    ++ toList
      ( ("--test=" <>)
          <$> test
      )

runGhcid :: Text -> Text -> Maybe Text -> IO ()
runGhcid cabalfile target test = do
  runProcess_ "rm -f .ghc.*"
  runProcess_ $ setWorkingDir "hsnrm" $ shell "cp -f $CABALFILE hsnrm.cabal"
  executeFile "ghcid" True (toS <$> ghcidTarget cabalfile target test) Nothing

main :: IO ()
main = do
  cabalFile <- toS <$> getEnv "CABALFILE"
  SIO.hSetBuffering SIO.stdout SIO.NoBuffering
    <> void (join (execParser (info (opts cabalFile <**> helper) idm)))
  where
    opts :: Text -> Parser (IO ())
    opts cabalFile =
      hsubparser
        ( OA.command
            "ghcid"
            ( info
                (runGhcid cabalFile <$> targetParser <*> testParser)
                (progDesc "Run an argo-compatible nix-build.")
            )
            <> OA.command
              "britt"
              (info (pure runbritt) (progDesc "inplace brittany."))
            <> OA.command
              "notebooks"
              (info (pure notebooks) (progDesc "Execute and convert notebooks."))
            <> OA.command
              "cabal"
              (info (pure cabal) (progDesc "generate cabal file."))
            <> OA.command
              "cabalstatic"
              (info (pure cabalstatic) (progDesc "generate cabal file for static build."))
            <> OA.command "shake" (info (pure (runshake [])) (progDesc "run shake."))
            <> OA.command
              "build"
              ( info
                  (pure (runshake ["build"]))
                  (progDesc "run shake for cabal build.")
              )
            <> OA.command
              "pyclient"
              ( info
                  (pure (runshake ["pyclient"]))
                  (progDesc "run shake for cabal build.")
              )
            <> OA.command
              "codegen"
              ( info
                  (pure (runshake ["codegen"]))
                  (progDesc "run shake for cabal build.")
              )
            <> OA.command
              "doc"
              ( info
                  (pure (runshake ["doc"]))
                  (progDesc "run shake for cabal build.")
              )
            <> OA.command
              "client"
              ( info
                  (pure (runshake ["client"]))
                  (progDesc "run shake for cabal build.")
              )
            <> help "Type of operation to run."
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
  mapM glob ["*.hs", "*/*.hs", "*/*/*.hs", "*/*/*.hs"] <&> concat
    >>= mapM_
      (\fn -> runProcess_ $ shell ("brittany --write-mode inplace " <> toS fn))

cabal = runProcess_ $ shell "dhall-to-cabal ./dev/pkgs/hsnrm/dev.dhall --output-dir-cwd hsnrm"

cabalstatic = runProcess_ $ shell "dhall-to-cabal ./dev/pkgs/hsnrm/static.dhall --output-dir-cwd hsnrm"

notebooks = do
  runProcess_ $ shell "notebooks/batchnb.py notebooks/configuration.ipynb"
  runProcess_ $ shell "notebooks/batchnb.py notebooks/tutorial.ipynb"
  runProcess_ $ shell "jupyter nbconvert doc/notebooks/notebooks/configuration.ipynb --output-dir=doc/notebooks/notebooks"
  runProcess_ $ shell "rm doc/notebooks/notebooks/configuration.ipynb"
  runProcess_ $ shell "rm doc/notebooks/notebooks/tutorial.ipynb"
  runProcess_ $ shell "jupyter nbconvert notebooks/tutorial.ipynb --output-dir=doc/notebooks/notebooks"

runshake as =
  withArgs as $ shakeArgs shakeOptions $ do
    phony "build" $ do
      version <- liftIO $ toS . strip . toS <$> readProcessStdout_ "ghc --numeric-version"
      ghcPathRaw <- liftIO $ strip . toS <$> readProcessStdout_ "which ghc"
      let ghcPath = dropEnd 8 ghcPathRaw
      liftIO (runProcess_ $ setWorkingDir "hsnrm" $ shell "cp -f $CABALFILE hsnrm.cabal")
      liftIO
        ( runProcess_ $ setWorkingDir "hsnrm" $
            proc
              "cabal"
              [ "v2-build",
                "nrm.so",
                "--ghc-option=-lHSrts_thr-ghc" <> version,
                "--ghc-option=-L" <> toS ghcPath <> "/lib/ghc-" <> version <> "/rts/",
                "--builddir=../.build",
                "--jobs=4"
              ]
        )
    phony "pyclient" $ do
      version <- liftIO $ toS . strip . toS <$> readProcessStdout_ "ghc --numeric-version"
      ghcPathRaw <- liftIO $ strip . toS <$> readProcessStdout_ "which ghc"
      let ghcPath = dropEnd 8 ghcPathRaw
      liftIO
        ( runProcess_ $ setWorkingDir "hsnrm" $
            proc
              "cabal"
              [ "v2-build",
                "pynrm.so",
                "--ghc-option=-lHSrts_thr-ghc" <> version,
                "--ghc-option=-L" <> toS ghcPath <> "/lib/ghc-" <> version <> "/rts/",
                "--builddir=../.build",
                "--jobs=4"
              ]
        )
    phony "codegen" $
      liftIO
        ( runProcess_ $ setWorkingDir "hsnrm" $
            proc
              "cabal"
              [ "v2-run",
                "--builddir=../.build_codegen",
                "codegen",
                "../resources/"
              ]
        )
    phony "client" $
      liftIO
        ( runProcess_ $ setWorkingDir "hsnrm" $
            proc
              "cabal"
              [ "v2-build",
                "nrm",
                "--builddir=../.build"
              ]
        )
    phony "doc" $ do
      (exitCode, out) <-
        liftIO
          ( readProcessStdout $ setWorkingDir "hsnrm" $
              proc
                "cabal"
                [ "v2-haddock",
                  "nrm.so",
                  "--haddock-hyperlink-source",
                  "--haddock-internal",
                  "--builddir=../.build",
                  "--haddock-html-location=\"https://hackage.haskell.org/package/\\$pkg-\\$version/docs\""
                ]
          )
      putText $ toS out
      let path = Prelude.last $ splitOn " " (Prelude.last $ Data.Text.lines $ toS out)
      liftIO (runProcess_ . shell $ "rm -rf doc/nrm.so/haddocks")
      liftIO (runProcess_ . shell $ "cp -r " <> (dropFileName $ toS path) <> " doc/nrm.so/haddocks")
      putText $ "documentation generated in doc/nrm.so/haddocks"
