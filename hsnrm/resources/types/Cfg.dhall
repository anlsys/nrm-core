-- ******************************************************************************
--  Copyright 2019 UChicago Argonne, LLC.
--  (c.f. AUTHORS, LICENSE)
--
--  SPDX-License-Identifier: BSD-3-Clause
-- ******************************************************************************
--
--     this file is generated, modifications will be erased.
--

{ verbose : < Error | Info | Debug >
, logfile : Text
, hwloc : Text
, perf : Text
, argo_perf_wrapper : Text
, argo_nodeos_config : Text
, libnrmPath : Optional Text
, pmpi_lib : Text
, singularity : Bool
, dummy : Bool
, nodeos : Bool
, slice_runtime : < Singularity | Nodeos | Dummy >
, downstreamCfg : { downstreamBindAddress : Text }
, upstreamCfg :
    { upstreamBindAddress : Text, pubPort : Integer, rpcPort : Integer }
, raplCfg :
    Optional
      { raplPath : Text
      , raplActions : List { fromuW : Double }
      , referencePower : { fromuW : Double }
      }
, hwmonCfg : { hwmonEnabled : Bool, hwmonPath : Text }
, controlCfg :
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
                    { neHead :
                        List { actuatorID : Text, actuatorValue : Double }
                    , neTail :
                        List
                          (List { actuatorID : Text, actuatorValue : Double })
                    }
                }
            >
        }
    | FixedCommand : { fixedPower : { fromuW : Double } }
    >
, activeSensorFrequency : { fromHz : Double }
, extraStaticPassiveSensors :
    List
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
              < Cumulative | IntervalBased | CumulativeWithCapacity : Double >
          }
      }
, extraStaticActuators :
    List
      { mapKey : Text
      , mapValue :
          { actuatorBinary : Text
          , actuatorArguments : List Text
          , actions : List Double
          , referenceAction : Double
          }
      }
}
