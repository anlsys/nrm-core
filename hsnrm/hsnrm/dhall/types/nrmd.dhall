-- ******************************************************************************
--  Copyright 2019 UChicago Argonne, LLC.
--  (c.f. AUTHORS, LICENSE)
--
--  SPDX-License-Identifier: BSD-3-Clause
-- ******************************************************************************
--
-- types used by nrmd's configuration.
--
--
let types = ./types.dhall

let Verbosity =
    -- Daemon verbosity:
    --   Error: Only report errors
    --   Info: Be verbose
    --   Debug: Report all that can possibly be reported
      < Error | Info | Debug >

let SensorBehavior =
    -- Sensor Behavior.
      < Cumulative | IntervalBased | CumulativeWithCapacity : Double >

let Range =
    -- An inclusive range of values
      { lower : Double, upper : Double }

let DownstreamCfg =
    -- Configuration for the dowstream API
      { downstreamBindAddress : Text }

let UpstreamCfg =
    -- Configuration for the upstream API
      { upstreamBindAddress : Text, pubPort : Integer, rpcPort : Integer }

let Tag =
    -- Available tags for describing sensors. Tags are used for setting
    -- objective functions up.
      < TagPower
      | Rapl
      | DownstreamThreadSignal
      | DownstreamCmdSignal
      | Minimize
      | Maximize
      >

let Sensor =
    -- Configuration for an arbitrary sensor.
      { sensorBinary : Text
      , sensorArguments : List Text
      , range : Range
      , tags : List Tag
      , sensorBehavior : SensorBehavior
      }

let Actuator =
    -- Configuration for an arbitrary actuator.
      { actuatorBinary : Text
      , actuatorArguments : List Text
      , actions : List Double
      , referenceAction : Double
      }

let StaticActuatorKV =
    -- Key-value representation for an actuator.
      { staticActuatorKey : Text, staticActuatorValue : Actuator }

let PassiveSensorKV =
    -- Key-value representation for a passive sensor.
      { passiveSensorKey : Text, passiveSensorValue : Sensor }

let Hwmon = { hwmonEnabled : Bool, hwmonPath : Text }

let RaplCfg =
    -- Configuration for auto-discovered RAPL power sensors/actuators
      { raplPath : Text
      , raplActions : List types.Power
      , referencePower : types.Power
      }

let ActuatorValue =
    -- Actuator value configuration for the internal control loop configuration.
      { actuatorID : Text, actuatorValue : Double }

let Hint =
    -- Action space configuration for internal control loop.
      < Full
      | Only :
          { neHead : List ActuatorValue, neTail : List (List ActuatorValue) }
      >

let LearnCfg =
    -- Internal control loop algorithm type and hyperparameters.
      < Lagrange : { lagrange : Double }
      | Random : { seed : Integer }
      | Contextual : { horizon : Integer }
      >

let ControlCfg =
    -- Root control configuration.
    -- ControlCfg: configures an internal control loop
    -- ControlOff: bypass mode
      < ControlCfg :
          { minimumControlInterval : types.Time
          , minimumWaitInterval : types.Time
          , staticPower : types.Power
          , learnCfg : LearnCfg
          , speedThreshold : Double
          , referenceMeasurementRoundInterval : Integer
          , hint : Hint
          }
      | ControlOff
      >

let Cfg =
    -- The configuration type for nrmd.
      { verbose : Verbosity
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
      , downstreamCfg : DownstreamCfg
      , upstreamCfg : UpstreamCfg
      , raplCfg : Optional RaplCfg
      , hwmonCfg : Hwmon
      , controlCfg : ControlCfg
      , passiveSensorFrequency : types.Frequency
      , extraStaticPassiveSensors : List PassiveSensorKV
      , extraStaticActuators : List StaticActuatorKV
      }

let output =
      { Verbosity = Verbosity
      , UpstreamCfg = UpstreamCfg
      , DownstreamCfg = DownstreamCfg
      , Tag = Tag
      , SensorBehavior = SensorBehavior
      , Actuator = Actuator
      , PassiveSensorKV = PassiveSensorKV
      , PassiveSensor = Sensor
      , StaticActuatorKV = StaticActuatorKV
      , Range = Range
      , PassiveSensorCfg = Sensor
      , ActuatorValue = ActuatorValue
      , ActuatorCfg = Actuator
      , HwmonCfg = Hwmon
      , LearnCfg = LearnCfg
      , RaplCfg = RaplCfg
      , ControlCfg = ControlCfg
      , Hint = Hint
      , Cfg = Cfg
      }

in  types ⫽ output
