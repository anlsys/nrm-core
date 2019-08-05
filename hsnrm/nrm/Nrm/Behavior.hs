{-|
Module      : Nrm.Behavior
Description : Nrm Behavior
Copyright   : (c) UChicago Argonne, 2019
License     : BSD3
Maintainer  : fre@freux.fr
-}
module Nrm.Behavior
  (behave
  )
where

import Protolude
import Nrm.Types.Configuration
import Nrm.Types.Application
{-import Nrm.Types.Container-}

data Behavior = Reply ByteString | StartChild Command Arguments

data Event = Event

behave :: Cfg -> Event -> Behavior
behave _ _ = Reply "toto"
