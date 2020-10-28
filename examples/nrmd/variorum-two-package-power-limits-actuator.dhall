let types = ../../hsnrm/hsnrm/dhall/types/nrmd.dhall

let default = ../../hsnrm/hsnrm/dhall/defaults/nrmd.dhall

in      default
      ⫽ { extraStaticActuators =
          [ { staticActuatorKey = "example extra actuator"
            , staticActuatorValue =
                { actuatorBinary = "variorum-set-socket-power-limits-example"
                , actuatorArguments = [] : List Text
                , actions = [ 100.0, 150.0 ]
                , referenceAction = 100.0
                }
            }
          ]
        }
    : types.Cfg
