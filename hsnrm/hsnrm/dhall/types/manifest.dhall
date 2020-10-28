-- ******************************************************************************
--  Copyright 2019 UChicago Argonne, LLC.
--  (c.f. AUTHORS, LICENSE)
--
--  SPDX-License-Identifier: BSD-3-Clause
-- ******************************************************************************
let types = ./types.dhall

let Scheduler =
    -- This type was imported from NRM1: feature not present (yet)
      < FIFO | HPC | Other : Integer >

let Perfwrapper =
    -- Configuration for linux perf performance measurements.
      { perfFreq : types.Frequency, perfLimit : Integer }

let ImageType =
    -- This type was imported from NRM1: feature not present (yet)
      < Sif | Docker >

let Image =
    -- This type was imported from NRM1: feature not present (yet)
      { path : Text, imagetype : ImageType, binds : Optional (List Text) }

let PowerPolicy =
    -- This type was imported from NRM1: feature not present (yet)
      < NoPowerPolicy | DDCM | DVFS | Combined >

let Slice =
    -- This type was imported from NRM1: feature not present (yet)
      { cpus : Integer, mems : Integer }

let PowerCfg =
    -- This type was imported from NRM1: feature not present (yet)
      { policy : PowerPolicy, profile : Bool, slowdown : Integer }

let Instrumentation =
    -- Message rate limitation for libnrm instrumentation.
      { ratelimit : types.Frequency }

let App -- Application configuration
        =
      { slice : Slice
      , scheduler : Scheduler
      , perfwrapper : Optional Perfwrapper
      , powerCfg : PowerCfg
      , instrumentation : Optional Instrumentation
      }

let Manifest =
    -- A manifest has a name, an application configuration.
    -- placeholders field from NRM1: hwbind, image
      { name : Text, app : App, hwbind : Bool, image : Optional Image }

in    types
    ⫽ { Scheduler = Scheduler
      , Perfwrapper = Perfwrapper
      , Image = Image
      , ImageType = ImageType
      , PowerPolicy = PowerPolicy
      , Slice = Slice
      , PowerCfg = PowerCfg
      , Instrumentation = Instrumentation
      , App = App
      , Manifest = Manifest
      }
