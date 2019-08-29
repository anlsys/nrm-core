{-|
Module      : Nrm.Orphans.ExitCode
Copyright   : (c) UChicago Argonne, 2019
License     : BSD3
Maintainer  : fre@freux.fr
-}
module Nrm.Orphans.ExitCode
  (
  )
where

import Data.MessagePack
import Protolude

instance MessagePack ExitCode where

  toObject ExitSuccess = toObject (0 :: Int)
  toObject (ExitFailure i) = toObject i

  fromObject x = fromObject x <&> \y -> if y == 0 then ExitSuccess else ExitFailure y
