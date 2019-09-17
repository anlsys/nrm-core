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
    < Normal | Verbose | Debug >.Normal
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
    { raplEnabled =
        True
    , raplPath =
        "/sys/devices/virtual/powercap/intel-rapl"
    , raplFrequency =
        { fromHz = 1.0 }
    }
, hwmonCfg =
    { hwmonEnabled = True, hwmonPath = "/sys/class/hwmon" }
}
