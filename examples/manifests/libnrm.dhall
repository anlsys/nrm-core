let types = ../../hsnrm/hsnrm/dhall/types/manifest.dhall

let default = ../../hsnrm/hsnrm/dhall/defaults/manifest.dhall

in      default
      ⫽ { name = "libnrm-wrapped"
        , app =
              default.app
            ⫽ { instrumentation = Some { ratelimit.hertz = 1000000.0 } }
        }
    : types.Manifest
