let types = ../../hsnrm/hsnrm/dhall/types/nrmd.dhall

let default = ../../hsnrm/hsnrm/dhall/defaults/nrmd.dhall

in      default
      ⫽ { extraStaticPassiveSensors =
          [ { passiveSensorKey =
                "Sensor that gets package power limits for package 0 through variorum"
            , passiveSensorValue =
                { sensorBinary = "bash"
                , sensorArguments =
                  [ "-c"
                  , "variorum-print-power-limits-example | awk '{ if (\$1 == \"_PACKAGE_ENERGY_STATUS\" && \$2 == \"0x610\" && \$4 == 0 ) { print \$6 } }'"
                  ]
                , range = { lower = 1.0, upper = 40.0 }
                , tags = [ types.Tag.TagPower ]
                , sensorBehavior = types.SensorBehavior.Cumulative
                }
            }
          , { passiveSensorKey =
                "Sensor that gets package power limits for package 1 through variorum"
            , passiveSensorValue =
                { sensorBinary = "bash"
                , sensorArguments =
                  [ "-c"
                  , "variorum-print-power-limits-example | awk '{ if (\$1 == \"_PACKAGE_ENERGY_STATUS\" && \$2 == \"0x610\" && \$4 == 1 ) { print \$6 } }'"
                  ]
                , range = { lower = 1.0, upper = 40.0 }
                , tags = [ types.Tag.TagPower ]
                , sensorBehavior = types.SensorBehavior.Cumulative
                }
            }
          ]
        }
    : types.Cfg
