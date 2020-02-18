Cfg
  { verbose = Info,
    logfile = "/tmp/nrm.log",
    hwloc = "hwloc",
    perf = "perf",
    argo_perf_wrapper = Command "nrm-perfwrapper",
    argo_nodeos_config = Command "argo_nodeos_config",
    libnrmPath = Nothing,
    pmpi_lib = "pmpi_lib",
    singularity = False,
    dummy = True,
    nodeos = False,
    slice_runtime = Dummy,
    downstreamCfg = DownstreamCfg {downstreamBindAddress = "ipc:///tmp/nrm-downstream-event"},
    upstreamCfg = UpstreamCfg
      { upstreamBindAddress = "*",
        pubPort = 2345,
        rpcPort = 3456
      },
    raplCfg =
      Just
        ( RaplCfg
            { raplPath = "/sys/devices/virtual/powercap/intel-rapl",
              raplFrequency = 1.0
            }
        ),
    hwmonCfg = HwmonCfg
      { hwmonEnabled = True,
        hwmonPath = "/sys/class/hwmon"
      },
    controlCfg = ControlCfg
      { minimumControlInterval = 1000000.0,
        staticPower = 2.0e8,
        learnCfg = Lagrange {lagrangeConstraint = LagrangeMultiplier 1.0},
        speedThreshold = Refined 0.9,
        referenceMeasurementRoundInterval = Refined 6
      }
  }
