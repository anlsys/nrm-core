-- ******************************************************************************
--  Copyright 2019 UChicago Argonne, LLC.
--  (c.f. AUTHORS, LICENSE)
--
--  SPDX-License-Identifier: BSD-3-Clause
-- ******************************************************************************
--
--     this file is generated, modifications will be erased.
--

{ name :
    Text
, app :
    { slice :
        { cpus : Integer, mems : Integer }
    , scheduler :
        < FIFO | HPC | Other : { _1 : Integer } >
    , perfwrapper :
        < PerfwrapperDisabled
        | Perfwrapper :
            { perfFreq :
                { fromHz : Double }
            , perfLimit :
                { fromOps : Integer }
            }
        >
    , power :
        { policy :
            < NoPowerPolicy | DDCM | DVFS | Combined >
        , profile :
            Bool
        , slowdown :
            Integer
        }
    , monitoring :
        { ratelimit : { fromHz : Double } }
    }
, hwbind :
    Bool
, image :
    < Image :
        { path :
            Text
        , magetype :
            < Sif | Docker >
        , binds :
            Optional (List Text)
        }
    | NoImage
    >
}
