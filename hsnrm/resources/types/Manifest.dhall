-- ******************************************************************************
--  Copyright 2019 UChicago Argonne, LLC.
--  (c.f. AUTHORS, LICENSE)
--
--  SPDX-License-Identifier: BSD-3-Clause
-- ******************************************************************************
--
--     this file is generated, modifications will be erased.
--

{ name : Text
, app :
    { slice : { cpus : Integer, mems : Integer }
    , scheduler : < FIFO | HPC | Other : Integer >
    , perfwrapper :
        < PerfwrapperDisabled
        | Perfwrapper :
            { perfFreq : { fromHz : Double }
            , perfLimit : { fromOps : Integer }
            }
        >
    , power :
        { policy : < NoPowerPolicy | DDCM | DVFS | Combined >
        , profile : Bool
        , slowdown : Integer
        }
    , instrumentation : Optional { ratelimit : { fromHz : Double } }
    }
, hwbind : Bool
, image :
    Optional
      { path : Text
      , imagetype : < Sif | Docker >
      , binds : Optional (List Text)
      }
}
