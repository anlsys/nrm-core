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
  )
where

import Dhall
import qualified Dhall
import qualified Dhall.Core as Dhall
import qualified Dhall.Parser
import qualified Dhall.TypeCheck as Dhall
import qualified Nrm.Types.Configuration as I
import Protolude

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
    )
  where
    cfg :: Dhall.Type I.Cfg
    cfg = Dhall.auto
    downstreamCfg :: Dhall.Type I.DownstreamCfg
    downstreamCfg = Dhall.auto

typeFile :: KnownType -> FilePath
typeFile = \case
  Cfg -> "types/Cfg.dhall"

generateDhall :: IO ()
generateDhall =
  putText "Generating types..."

  for_ [ minBound .. maxBound ] $ \ knownType -> do
    let
      localDest =
        typeFile knownType

      expr =
        importFile . relativeTo localDest . typeFile <$> factored knownType

      dest =
        prefix <> "/" <> localDest

    putStrLn $
      "  Writing type for " ++ show knownType ++ " to " ++ dest ++ "."

    createDirectoryIfMissing True ( takeDirectory dest )

    writeOutput dest expr
    where prefix = "resources/"
