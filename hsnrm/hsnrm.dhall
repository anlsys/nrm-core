let prelude = ./dhall-to-cabal/prelude.dhall

let types = ./dhall-to-cabal/types.dhall

let common = ./common.dhall

in    λ ( ghcPath
        : Text
        )
    → λ(ghcNumericVersion : Text)
    →   prelude.defaults.Package
      ⫽ { name =
            "hsnrm"
        , version =
            prelude.v "1.0.0"
        , author =
            "Valentin Reis"
        , build-type =
            Some types.BuildType.Simple
        , cabal-version =
            prelude.v "2.0"
        , category =
            "tools"
        , description =
            "The Node Resource Manager(NRM) is a linux daemon that enables dynamic resource optimization for improving the power/performance tradeoff of HPC applications."
        , library =
            prelude.unconditional.library
            (   prelude.defaults.Library
              ⫽ { build-depends =
                    common.libdep
                , hs-source-dirs =
                    [ "nrm" ]
                , exposed-modules =
                    common.allmodules
                }
              ⫽ common.copts ([] : List Text)
            )
        , executables =
            [ { executable =
                    λ(config : types.Config)
                  →   prelude.defaults.Executable
                    ⫽ { main-is =
                          "bin/Export.hs"
                      , build-depends =
                          [ common.nobound "hsnrm"
                          , common.deps.protolude
                          , common.deps.base
                          ]
                      }
                    ⫽ common.copts
                      [ "-fPIC", "-shared", "-no-hs-main", "-dynamic" ]
              , name =
                  "nrm.so"
              }
            , { executable =
                    λ(config : types.Config)
                  →   prelude.defaults.Executable
                    ⫽ { main-is =
                          "bin/PyExport.hs"
                      , build-depends =
                          [ common.nobound "hsnrm"
                          , common.deps.bytestring
                          , common.deps.aeson
                          , common.deps.protolude
                          , common.deps.base
                          , common.deps.data-default
                          , common.deps.zeromq4-haskell
                          , common.deps.pretty-simple
                          ]
                      }
                    ⫽ common.copts
                      [ "-fPIC", "-shared", "-no-hs-main", "-dynamic" ]
              , name =
                  "pynrm.so"
              }
            , { executable =
                    λ(config : types.Config)
                  →   prelude.defaults.Executable
                    ⫽ { main-is =
                          "bin/Hnrm.hs"
                      , build-depends =
                          [ common.nobound "hsnrm" ]
                      }
                    ⫽ common.copts [ "-main-is", "Hnrm" ]
              , name =
                  "nrm"
              }
            ]
        , extra-source-files =
            [ "ChangeLog.md" ]
        , license =
            types.License.BSD3
        , license-files =
            [] : List Text
        , maintainer =
            "fre@freux.fr"
        , source-repos =
            [   prelude.defaults.SourceRepo
              ⫽ { type =
                    Some types.RepoType.Git
                , location =
                    Some "https://xgitlab.cels.anl.gov/vreis/hsnrm.git"
                }
            ]
        , synopsis =
            "hsnrm"
        }
