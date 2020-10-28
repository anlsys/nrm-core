let t = ../types/nrmd.dhall

in    { verbose = t.Verbosity.Error
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
      , downstreamCfg.downstreamBindAddress = "ipc:///tmp/nrm-downstream-event"
      , upstreamCfg =
          { upstreamBindAddress = "*", pubPort = +2345, rpcPort = +3456 }
      , raplCfg = Some
          { raplPath = "/sys/devices/virtual/powercap/intel-rapl"
          , raplActions = [ { microwatts = 1.0e8 }, { microwatts = 2.0e8 } ]
          , referencePower.microwatts = 2.5e8
          }
      , hwmonCfg = { hwmonEnabled = True, hwmonPath = "/sys/class/hwmon" }
      , controlCfg = t.ControlCfg.ControlOff
      , passiveSensorFrequency.hertz = 1.0
      , extraStaticPassiveSensors = [] : List t.SensorKV
      , extraStaticActuators = [] : List t.ActuatorKV
      }
    : t.Cfg
