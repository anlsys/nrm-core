-- ******************************************************************************
--  Copyright 2019 UChicago Argonne, LLC.
--  (c.f. AUTHORS, LICENSE)
--
--  SPDX-License-Identifier: BSD-3-Clause
-- ******************************************************************************
--
--     this file is generated, modifications will be erased.
--

{ verbose =
    < Error | Info | Debug >.Error
, logfile =
    "/tmp/nrm.log"
, hwloc =
    "hwloc"
, perf =
    "perf"
, argo_perf_wrapper =
    "nrm-perfwrapper"
, argo_nodeos_config =
    "argo_nodeos_config"
, libnrmPath =
    None Text
, pmpi_lib =
    "pmpi_lib"
, singularity =
    False
, dummy =
    True
, nodeos =
    False
, slice_runtime =
    < Singularity | Nodeos | Dummy >.Dummy
, downstreamCfg =
    { downstreamBindAddress = "ipc:///tmp/nrm-downstream-event" }
, upstreamCfg =
    { upstreamBindAddress = "*", pubPort = +2345, rpcPort = +3456 }
, raplCfg =
    None { raplPath : Text, raplFrequency : { fromHz : Double } }
, hwmonCfg =
    { hwmonEnabled = True, hwmonPath = "/sys/class/hwmon" }
, controlCfg =
    None
    { minimumControlInterval :
        { fromuS : Double }
    , learnCfg :
        < LagrangeConstraints :
            { _1 : { _1 : Double } }
        | KnapsackConstraints :
            { _1 : { _1 : Double } }
        >
    , speedThreshold :
        Double
    , referenceMeasurementRoundInterval :
        Integer
    }
}
