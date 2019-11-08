{-# OPTIONS_GHC -fno-warn-orphans #-}

-- |
-- Module      : NRM.Manifest.Examples
-- Copyright   : (c) 2019, UChicago Argonne, LLC.
-- License     : BSD3
-- Maintainer  : fre@freux.fr
module NRM.Manifest.Examples
  (
  )
where

import Data.Default
import Data.Map
import NRM.Classes.Examples
import NRM.Types.Manifest
import NRM.Types.Units as U
import Protolude

instance Examples Manifest where
  examples =
    fromList
      [ ("default", def),
        ( "perfwrap",
          def
            { app =
                def
                  { perfwrapper = Perfwrapper $ MkPw
                      { perfFreq = U.hz 1,
                        perfLimit = U.Operations 100000
                      }
                  }
            }
        ),
        ( "libnrm",
          def
            { app =
                def
                  { instrumentation = Just $ Instrumentation
                      { ratelimit = U.hz 1000000
                      }
                  }
            }
        )
      ]
