-- |
-- Module      : NRM.Daemon
-- Copyright   : (c) 2019, UChicago Argonne, LLC.
-- License     : BSD3
-- Maintainer  : fre@freux.fr
module NRM.Daemon
  ( main,
  )
where

import NRM.Optparse (parseDaemonCli)
import Protolude

-- | The main user facing nrm daemon process
main :: IO ()
main = do
  req <- parseDaemonCli
  print req
