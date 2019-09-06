{-|
Module      : Bandit.Exp3
Copyright   : (c) 2019, UChicago Argonne, LLC.
License     : MIT
Maintainer  : fre@freux.fr
-}
module Bandit.Exp3
  ( Exp3 (..)
  )
where

import Bandit.Class
{-import System.Random-}
import Protolude

data Exp3 a
  = Exp3
      { t :: Int
      , a :: Maybe a
      , ws :: [Double]
      }

instance Bandit (Exp3 a) a reward where

  initialBandit k = Exp3
    { t = 1
    , a = Nothing
    , ws = replicate k 1
    }

  step _b = panic "Exp.3 not implemented."
