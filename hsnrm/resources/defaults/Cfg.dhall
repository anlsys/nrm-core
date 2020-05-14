-- ******************************************************************************
--  Copyright 2019 UChicago Argonne, LLC.
--  (c.f. AUTHORS, LICENSE)
--
--  SPDX-License-Identifier: BSD-3-Clause
-- ******************************************************************************
--
--     this file is generated, modifications will be erased.
--

{ verbose = < Error | Info | Debug >.Error
, logfile = "/tmp/nrm.log"
, hwloc = "hwloc"
, perf = "perf"
, argo_perf_wrapper = "nrm-perfwrapper"
, argo_nodeos_config = "argo_nodeos_config"
, libnrmPath = None Text
, pmpi_lib = "pmpi_lib"
, singularity = False
, dummy = True
, nodeos = False
, slice_runtime = < Singularity | Nodeos | Dummy >.Dummy
, downstreamCfg = { downstreamBindAddress = "ipc:///tmp/nrm-downstream-event" }
, upstreamCfg = { upstreamBindAddress = "*", pubPort = +2345, rpcPort = +3456 }
, raplCfg = Some
    { raplPath = "/sys/devices/virtual/powercap/intel-rapl"
    , raplActions = [ { fromuW = 1.0e8 }, { fromuW = 2.0e8 } ]
    , referencePower = { fromuW = 2.5e8 }
    }
, hwmonCfg = { hwmonEnabled = True, hwmonPath = "/sys/class/hwmon" }
, controlCfg =
    < ControlCfg :
        { minimumControlInterval : { fromuS : Double }
        , staticPower : { fromuW : Double }
        , learnCfg :
            < Lagrange : { lagrange : Double }
            | Random : { random : Optional Integer }
            | Contextual : { contextual : { horizon : Integer } }
            >
        , speedThreshold : Double
        , referenceMeasurementRoundInterval : Integer
        , hint :
            < Full
            | Only :
                { only :
                    List (List { actuatorID : Text, actuatorValue : Double })
                }
            >
        }
    | FixedCommand : { fixedPower : { fromuW : Double } }
    >.FixedCommand
      { fixedPower = { fromuW = 2.5e8 } }
, activeSensorFrequency = { fromHz = 1.0 }
, extraStaticPassiveSensors =
    [] : List
           { mapKey : Text
           , mapValue :
               { sensorBinary : Text
               , sensorArguments : List Text
               , range : < I : { _1 : Double, _2 : Double } | Empty >
               , tags :
                   List
                     < Power
                     | Rapl
                     | DownstreamThreadSignal
                     | DownstreamCmdSignal
                     | Minimize
                     | Maximize
                     >
               , sensorBehavior :
                   < Cumulative
                   | IntervalBased
                   | CumulativeWithCapacity : Double
                   >
               }
           }
, extraStaticActuators =
    [] : List
           { mapKey : Text
           , mapValue :
               { actuatorBinary : Text
               , actuatorArguments : List Text
               , actions : List Double
               , referenceAction : Double
               }
           }
}
