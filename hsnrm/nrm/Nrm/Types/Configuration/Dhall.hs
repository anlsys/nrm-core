{-|
Module      : Nrm.Types.Configuration.Dhall
Copyright   : (c) UChicago Argonne, 2019
License     : BSD3
Maintainer  : fre@freux.fr
-}
module Nrm.Types.Configuration.Dhall
  ( inputCfg
  , toInternal
  , fromInternal
  , I.Cfg (..)
  , I.DaemonVerbosity (..)
  , I.ContainerRuntime (..)
  , I.UpstreamCfg (..)
  , I.DownstreamCfg (..)
  , -- * Code generation
    generateDhall
  )
where

import Codegen.Dhall
import Dhall
{-import qualified Dhall-}
import qualified Dhall.Core as Dhall
import qualified Dhall.Parser
import qualified Dhall.TypeCheck as Dhall
import qualified Nrm.Types.Configuration as I
import Protolude
import System.Directory
import System.FilePath
  ( {-  (<.>)-}
    {-, (</>)-}
    {-, dropTrailingPathSeparator-}
    {-, normalise-}
    {-, splitDirectories-}
    {-, splitFileName-}
    takeDirectory
  )

-- As soon as Internal.Cfg isn't Interpretable, we write a dhall
-- interpretable datatype layer here. As it stands, this is a transitive
-- "identity" placeholder.
inputDCfg :: (MonadIO m) => Text -> m I.Cfg
inputDCfg fn =
  liftIO $ try (input dt fn) >>= \case
    Right d -> return d
    Left e -> throwError e

dt :: Dhall.Type I.Cfg
dt = Dhall.auto

toInternal :: I.Cfg -> I.Cfg
toInternal = identity

fromInternal :: I.Cfg -> I.Cfg
fromInternal = identity

inputCfg :: (MonadIO m) => Text -> m I.Cfg
inputCfg fn = toInternal <$> inputDCfg fn

data KnownType
  = Cfg
  | DownstreamCfg
  deriving (Bounded, Enum, Eq, Ord, Read, Show)

dhallType :: KnownType -> Dhall.Expr Dhall.Parser.Src a
dhallType t =
  fmap Dhall.absurd
    ( case t of
      Cfg -> Dhall.expected cfg
      DownstreamCfg -> Dhall.expected downstreamCfg
    )
  where
    cfg :: Dhall.Type I.Cfg
    cfg = Dhall.auto
    downstreamCfg :: Dhall.Type I.DownstreamCfg
    downstreamCfg = Dhall.auto

typeFile :: KnownType -> FilePath
typeFile = \case
  Cfg -> "types/Cfg.dhall"
  DownstreamCfg -> "types/Cfg.dhall"

generateDhall :: IO ()
generateDhall = do
  putText "Generating types..."
  for_ [minBound .. maxBound] $ \knownType -> do
    let localDest =
          typeFile knownType
        expr = dhallType knownType
        dest =
          prefix <> "/" <> localDest
    putText $ "  Writing type for " <> show knownType <> " to " <> toS dest <> "."
    createDirectoryIfMissing True (takeDirectory dest)
    writeOutput dest expr
  where
    prefix = "resources/"
