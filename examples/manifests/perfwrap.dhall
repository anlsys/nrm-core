let types = ../../hsnrm/hsnrm/dhall/types/manifest.dhall

let default = ../../hsnrm/hsnrm/dhall/defaults/manifest.dhall

in      default
      ⫽ { name = "linuxperf-wrapped"
        , app =
              default.app
            ⫽ { perfwrapper = Some { perfFreq.hertz = 1.0,
	    			     perfLimit = +100000,
				     perfEvent = "instructions" }
              }
        }
    : types.Manifest
