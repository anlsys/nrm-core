-- ******************************************************************************
--  Copyright 2019 UChicago Argonne, LLC.
--  (c.f. AUTHORS, LICENSE)
--
--  SPDX-License-Identifier: BSD-3-Clause
-- ******************************************************************************
--
--     this file is generated, modifications will be erased.
--

{ name =
    "default"
, app =
    { slice =
        { cpus = +1, mems = +1 }
    , scheduler =
        < FIFO | HPC | Other : { _1 : Integer } >.FIFO
    , perfwrapper =
        < PerfwrapperDisabled
        | Perfwrapper :
            { _1 :
                { perfFreq :
                    { fromHz : Double }
                , perfLimit :
                    { fromOps : Integer }
                }
            }
        >.PerfwrapperDisabled
    , power =
        { policy =
            < NoPowerPolicy | DDCM | DVFS | Combined >.NoPowerPolicy
        , profile =
            False
        , slowdown =
            +1
        }
    , instrumentation =
        Some
        { ratelimit = { fromHz = 1000000.0 }, libnrmPath = "~/lib/libnrm.so" }
    }
, hwbind =
    False
, image =
    None
    { path : Text, magetype : < Sif | Docker >, binds : Optional (List Text) }
}
