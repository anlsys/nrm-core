let prelude = ../dhall-to-cabal/prelude.dhall

let types = ../dhall-to-cabal/types.dhall

let common = ../common.dhall

in    λ(ghcPath : Text)
    → λ(ghcNumericVersion : Text)
    →   prelude.defaults.Package
      ⫽ { name = "hsnrm-extra"
        , version = prelude.v "1.0.0"
        , author = "Valentin Reis"
        , build-type = Some types.BuildType.Simple
        , cabal-version = prelude.v "2.0"
        , category = "tools"
        , description = "hsnrm utilities"
        , executables =
          [ { executable =
                  λ(config : types.Config)
                →   prelude.defaults.Executable
                  ⫽ { main-is = "PyExport.hs"
                    , build-depends =
                      [ common.nobound "hsnrm"
                      , common.deps.base
                      , common.deps.protolude
                      , common.deps.containers
                      , common.deps.aeson
                      , common.deps.zeromq4-haskell
                      , common.deps.pretty-simple
                      , common.deps.data-default
                      , common.deps.bytestring
                      , common.deps.enclosed-exceptions
                      ]
                    , extra-lib-dirs =
                      [ ghcPath ++ "/lib/ghc-" ++ ghcNumericVersion ++ "/rts/" ]
                    }
                  ⫽ common.copts
                      [ "-Wmissed-specialisations"
                      , "-Wall-missed-specialisations"
                      , "-fPIC"
                      , "-shared"
                      , "-no-hs-main"
                      , "-dynamic"
                      , "-lHSrts-ghc" ++ ghcNumericVersion
                      ]
            , name = "pynrm.so"
            }
          ]
        , extra-source-files = [] : List Text
        , license = types.License.BSD3
        , license-files = [] : List Text
        , maintainer = "fre@freux.fr"
        , source-repos =
          [   prelude.defaults.SourceRepo
            ⫽ { type = Some types.RepoType.Git
              , location = Some "https://xgitlab.cels.anl.gov/vreis/hsnrm.git"
              }
          ]
        , synopsis = "hsnrm-extra"
        }
