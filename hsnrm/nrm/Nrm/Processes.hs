{-|
Module      : Nrm.Processes
Copyright   : (c) UChicago Argonne, 2019
License     : BSD3
Maintainer  : fre@freux.fr
-}
module Nrm.Processes
  ( signalProcess
  )
where

import Nrm.Types.Process
import Protolude
import qualified System.Posix.Signals as S

signalProcess :: S.Signal -> ProcessID -> IO ()
signalProcess sig (ProcessID x) = S.signalProcess sig x
