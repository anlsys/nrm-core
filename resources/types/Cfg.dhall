-- ******************************************************************************
--  Copyright 2019 UChicago Argonne, LLC.
--  (c.f. AUTHORS, LICENSE)
--
--  SPDX-License-Identifier: BSD-3-Clause
-- ******************************************************************************
--
--     this file is generated, modifications will be erased.
--

{ verbose :
    < Normal | Verbose | Debug >
, logfile :
    Text
, hwloc :
    Text
, perf :
    Text
, argo_perf_wrapper :
    Text
, argo_nodeos_config :
    Text
, libnrmPath :
    Optional Text
, pmpi_lib :
    Text
, singularity :
    Bool
, dummy :
    Bool
, nodeos :
    Bool
, slice_runtime :
    < Singularity | Nodeos | Dummy >
, downstreamCfg :
    { downstreamBindAddress : Text }
, upstreamCfg :
    { upstreamBindAddress : Text, pubPort : Integer, rpcPort : Integer }
, raplCfg :
    { raplEnabled : Bool, raplPath : Text, raplFrequency : { fromHz : Double } }
, hwmonCfg :
    { hwmonEnabled : Bool, hwmonPath : Text }
, maximumControlFrequency :
    { fromHz : Double }
}
