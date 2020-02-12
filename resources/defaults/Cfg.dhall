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
    Some
    { raplPath =
        "/sys/devices/virtual/powercap/intel-rapl"
    , raplFrequency =
        { fromHz = 1.0 }
    }
, hwmonCfg =
    { hwmonEnabled = True, hwmonPath = "/sys/class/hwmon" }
, controlCfg =
    None
    { minimumControlInterval :
        { fromuS : Double }
    , staticPower :
        { fromuW : Double }
    , learnCfg :
        < Lagrange :
            { lagrangeConstraint : Double }
        | Knapsack :
            { knapsackConstraint : Double }
        >
    , speedThreshold :
        Double
    , referenceMeasurementRoundInterval :
        Integer
    }
}
