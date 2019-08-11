{-# LANGUAGE NoImplicitPrelude #-}

{-|
Module      : export
Copyright   : (c) 2019, UChicago Argonne, LLC.
License     : BSD3
Maintainer  : fre@freux.fr
-}
module Nrm.Export
  ( parseDaemon
  , isVerbose
  )
where

import qualified Nrm.Optparse as O (parseArgDaemonCli)
import qualified Nrm.Types.Configuration as C (Cfg, DaemonVerbosity (..), verbose)
import Protolude

parseDaemon :: [Text] -> IO C.Cfg
parseDaemon = O.parseArgDaemonCli

isVerbose :: C.Cfg -> Bool
isVerbose c =
  C.Verbose == C.verbose c
