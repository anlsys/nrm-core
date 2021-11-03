-- |
-- Module      : NRM.Processes
-- Copyright   : (c) UChicago Argonne, 2019
-- License     : BSD3
-- Maintainer  : swann@anl.gov
module NRM.Processes
  ( signalProcess,
  )
where

import NRM.Types.Process
import Protolude
import qualified System.Posix.Signals as S

signalProcess :: S.Signal -> ProcessID -> IO ()
signalProcess sig (ProcessID x) = S.signalProcess sig x
