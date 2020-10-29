-- ******************************************************************************
--  Copyright 2019 UChicago Argonne, LLC.
--  (c.f. AUTHORS, LICENSE)
--
--  SPDX-License-Identifier: BSD-3-Clause
-- ******************************************************************************

let t = ../types/manifest.dhall

in  { name = "default"
    , app =
        { perfwrapper = None t.Perfwrapper
        , instrumentation = None t.Instrumentation
        }
    }
