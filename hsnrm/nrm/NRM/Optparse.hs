{-|
Module      : NRM.Optparse
Copyright   : (c) UChicago Argonne, 2019
License     : BSD3
Maintainer  : fre@freux.fr
-}
module NRM.Optparse
  ( parseDaemonCli
  , parseArgDaemonCli
  , parseClientCli
  )
where

import GHC.IO.Encoding
import qualified NRM.Optparse.Client as C
import qualified NRM.Optparse.Daemon as D
import NRM.Types.Configuration
import Options.Applicative
import Protolude
import qualified System.IO as SIO

customExecParserArgs :: [Text] -> ParserPrefs -> ParserInfo a -> IO a
customExecParserArgs args pprefs pinfo =
  handleParseResult $ execParserPure pprefs pinfo (toS <$> args)

parseDaemonCli :: IO Cfg
parseDaemonCli = fmap toS <$> getArgs >>= parseCli "nrm" "NRM Client" D.opts

parseArgDaemonCli :: [Text] -> IO Cfg
parseArgDaemonCli = parseCli "nrmd" "NRM Daemon" D.opts

parseClientCli :: IO C.Opts
parseClientCli = fmap toS <$> getArgs >>= parseCli "nrm" "NRM Client" C.opts

parseCli :: Text -> Text -> Parser (IO a) -> [Text] -> IO a
parseCli h d x args =
  GHC.IO.Encoding.setLocaleEncoding SIO.utf8 >>
    ( join .
      customExecParserArgs args (prefs showHelpOnError) $
      info
        (helper <*> x)
        ( fullDesc <> header (toS h) <>
          progDesc (toS d)
        )
    )
