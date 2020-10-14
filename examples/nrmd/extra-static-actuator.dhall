let types = ../../hsnrm/hsnrm/dhall/types/nrmd.dhall

let default = ../../hsnrm/hsnrm/dhall/defaults/nrmd.dhall

in      default
      ⫽ { extraStaticActuators =
          [ { staticActuatorKey = "example extra actuator ID"
            , staticActuatorValue =
                { actuatorBinary = "bash"
                , actuatorArguments =
                  [ "-c"
                  , "echo \$@ >> /tmp/test-nrm-example-extra-actuator"
                  , "-o"
                  ]
                , actions = [ 1.0, 2.0 ]
                , referenceAction = 1.0
                }
            }
          ]
        }
    : types.Cfg
