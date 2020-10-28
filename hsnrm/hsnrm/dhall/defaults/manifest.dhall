-- ******************************************************************************
--  Copyright 2019 UChicago Argonne, LLC.
--  (c.f. AUTHORS, LICENSE)
--
--  SPDX-License-Identifier: BSD-3-Clause
-- ******************************************************************************

let t = ../types/manifest.dhall

in  { name = "default"
    , app =
        { slice = { cpus = +1, mems = +1 }
        , scheduler = t.Scheduler.FIFO
        , perfwrapper = None t.Perfwrapper
        , powerCfg =
            { policy = < NoPowerPolicy | DDCM | DVFS | Combined >.NoPowerPolicy
            , profile = False
            , slowdown = +1
            }
        , instrumentation = None t.Instrumentation
        }
    , hwbind = False
    , image =
        None
          { path : Text
          , imagetype : < Sif | Docker >
          , binds : Optional (List Text)
          }
    }
