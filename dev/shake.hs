{-# LANGUAGE OverloadedStrings #-}
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
import System.Directory
import System.Environment (getEnv, withArgs)
import "Glob" System.FilePath.Glob
import qualified System.IO as SIO
  ( BufferMode (..)
  , hSetBuffering
  , stdout
  )
import System.Posix.Process
import System.Process.Typed

ghcidTarget :: Text -> Maybe Text -> [Text]
ghcidTarget target test =
  [ "-C"
  , "hsnrm"
  , "--command"
  , "cabal " <> "new-repl " <> target <> " --ghc-option=-fno-code"
  , "--restart=hsnrm.cabal"
  , "--restart=default.nix"
  , "--restart=shell.nix"
  ] ++
    toList
      ( ("--test=" <>) <$>
        test
      )

runGhcid :: Text -> Maybe Text -> IO ()
runGhcid target test = do
  runProcess_ "rm -f .ghc.*"
  executeFile "ghcid" True (toS <$> ghcidTarget target test) Nothing

main :: IO ()
main =
  SIO.hSetBuffering SIO.stdout SIO.NoBuffering <>
    void (join (execParser (info (opts <**> helper) idm)))
  where
    opts :: Parser (IO ())
    opts =
      hsubparser
        ( OA.command
          "ghcid"
          ( info (runGhcid <$> targetParser <*> testParser)
            (progDesc "Run an argo-compatible nix-build.")
          ) <>
          OA.command "britt"
            (info (pure runbritt) (progDesc "inplace brittany.")) <>
          OA.command "cabal"
            (info (pure cabal) (progDesc "generate cabal file.")) <>
          OA.command "shake" (info (pure (runshake [])) (progDesc "run shake.")) <>
          OA.command
            "readme"
            ( info (pure (runshake ["README.md"]))
              (progDesc "run shake for README.md.")
            ) <>
          OA.command
            "build"
            ( info (pure (runshake ["build"]))
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

cabal = runProcess_ $ shell "dhall-to-cabal ./dev/pkgs/hsnrm/default.dhall --output-dir-cwd hsnrm"

runshake as =
  withArgs as $ shakeArgs shakeOptions $
    phony "build" $ do
    version <- liftIO $ toS . strip . toS <$> readProcessStdout_ "ghc --numeric-version"
    ghcPathRaw <- liftIO $ strip . toS <$> readProcessStdout_ "which ghc"
    let ghcPath = dropEnd 8 ghcPathRaw
    liftIO
      ( runProcess_ $ setWorkingDir "hsnrm" $
        proc "cabal"
          [ "v2-build"
          , "nrm.so"
          , "--ghc-option=-lHSrts-ghc" <> version
          , "--ghc-option=-L" <> toS ghcPath <> "/lib/ghc-" <> version <> "/rts/"
          ]
      )
