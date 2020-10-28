let types = ../../hsnrm/hsnrm/dhall/types/nrmd.dhall

let default = ../../hsnrm/hsnrm/dhall/defaults/nrmd.dhall

in      default
      ⫽ { extraStaticPassiveSensors =
          [ { passiveSensorKey = "example extra static passive power sensor"
            , passiveSensorValue =
                { sensorBinary = "echo"
                , sensorArguments = [ "30" ]
                , range = { lower = 1.0, upper = 40.0 }
                , tags = [ types.Tag.TagPower ]
                , sensorBehavior = types.SensorBehavior.IntervalBased
                }
            }
          ]
        }
    : types.Cfg
