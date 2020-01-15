-- |
-- Module      : NRM.Optparse.Daemon
-- Copyright   : (c) UChicago Argonne, 2019
-- License     : BSD3
-- Maintainer  : fre@freux.fr
module NRM.Optparse.Daemon
  ( opts,
  )
where

import qualified Data.ByteString as B (getContents)
import Data.Default
import qualified Data.Text.Prettyprint.Doc as Pretty
import qualified Data.Text.Prettyprint.Doc.Render.Text as Pretty
import Dhall
import qualified Dhall.Core as Dhall
import qualified Dhall.Lint as Lint
import qualified Dhall.Src as Dhall
import qualified Dhall.TypeCheck as Dhall
import NRM.Types.Configuration
import qualified NRM.Types.Configuration as C
import qualified NRM.Types.Configuration.Yaml as Y
import Options.Applicative
import Protolude
import System.Directory
import System.FilePath.Posix
import Text.Editor
import qualified Prelude (print)

prettyOpts :: Pretty.LayoutOptions
prettyOpts =
  Pretty.defaultLayoutOptions
    { Pretty.layoutPageWidth = Pretty.AvailablePerLine 80 1.0
    }

data MainCfg
  = MainCfg
      { useStdin :: Bool,
        inputfile :: Maybe Text,
        stdinType :: SourceType,
        edit :: Bool
      }

commonParser :: Parser MainCfg
commonParser =
  MainCfg
    <$> flag
      False
      True
      (long "stdin" <> short 'i' <> help "Read configuration on stdin.")
    <*> optional
      ( strArgument
          ( metavar "CONFIG"
              <> help
                "Input configuration with .yml/.yaml/.dh/.dhall extension. Leave void for stdin (dhall) input."
          )
      )
    <*> flag
      Dhall
      Yaml
      ( long "yaml" <> short 'y'
          <> help
            "Assume stdin to be yaml instead of dhall."
      )
    <*> flag
      False
      True
      (long "edit" <> short 'e' <> help "Edit yaml in $EDITOR before running the NRM daemon.")

opts :: Parser (IO Cfg)
opts = (load <$> commonParser) <**> helper

data SourceType = Dhall | Yaml
  deriving (Eq)

data FinallySource = UseDefault | NoExt | FinallyFile SourceType Text | FinallyStdin SourceType

ext :: Bool -> SourceType -> Maybe Text -> FinallySource
ext _ _ (Just fn)
  | xt `elem` [".dh", ".dhall"] = FinallyFile Dhall fn
  | xt `elem` [".yml", ".yaml"] = FinallyFile Yaml fn
  | otherwise = NoExt
  where
    xt = takeExtension $ toS fn
ext useStdin st Nothing = if useStdin then FinallyStdin st else UseDefault

load :: MainCfg -> IO Cfg
load MainCfg {..} =
  (if edit then editing else return) =<< case ext useStdin stdinType inputfile of
    (FinallyFile Dhall filename) ->
      detailed $
        C.inputCfg
          =<< toS
          <$> makeAbsolute (toS filename)
    (FinallyFile Yaml filename) ->
      Y.decodeCfgFile =<< toS <$> makeAbsolute (toS filename)
    (FinallyStdin Yaml) ->
      B.getContents <&> Y.decodeCfg >>= \case
        Left e -> Prelude.print e >> die "yaml parsing exception."
        Right manifest -> return manifest
    (FinallyStdin Dhall) ->
      B.getContents >>= C.inputCfg
        . ( \s ->
              Pretty.renderStrict
                ( Pretty.layoutSmart
                    prettyOpts
                    ( Pretty.pretty
                        ( Lint.lint $ Dhall.absurd <$> embed (injectWith defaultInterpretOptions) (def :: Cfg) ::
                            Dhall.Expr Dhall.Src Dhall.Import
                        )
                    )
                )
                <> " // "
                <> toS s
          )
    UseDefault -> return def
    NoExt ->
      case inputfile of
        Nothing -> return def
        Just s ->
          C.inputCfg $
            Pretty.renderStrict
              ( Pretty.layoutSmart
                  prettyOpts
                  ( Pretty.pretty
                      ( Lint.lint $ Dhall.absurd <$> embed (injectWith defaultInterpretOptions) (def :: Cfg) ::
                          Dhall.Expr Dhall.Src Dhall.Import
                      )
                  )
              )
              <> " // "
              <> toS s

editing :: Cfg -> IO Cfg
editing c =
  runUserEditorDWIM yt (Y.encodeCfg c) <&> Y.decodeCfg >>= \case
    Left e -> Prelude.print e >> die "yaml parsing exception."
    Right cfg -> return cfg
  where
    yt = mkTemplate "yaml"
