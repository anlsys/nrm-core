let prelude = ./dhall-to-cabal/prelude.dhall

let types = ./dhall-to-cabal/types.dhall

let common = ./common.dhall

in    prelude.defaults.Package
    ⫽ { name =
          "hsnrm-static"
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
      ,sub-libraries =[
           { library =
                  λ(config : types.Config)
                →   prelude.defaults.Library
                  ⫽ { build-depends =
                        common.libdep
                    , hs-source-dirs =
                        [ "nrm", "bin" ]
                    , exposed-modules =
                        common.allmodules
                    }
                  ⫽ common.copts ([] : List Text)
            , name =
                "nrmlib"
            }]
      , executables =
          [ { executable =
                  λ(config : types.Config)
                →   prelude.defaults.Executable
                  ⫽ { main-is =
                        "Export.hs"
                    , build-depends =
                        [ common.nobound "nrmlib"
                        , common.deps.unix
                        , common.deps.base
                        , common.deps.protolude
                        , common.deps.pretty-simple
                        , common.deps.random-fu
                        , common.deps.random ]
                    , hs-source-dirs =
                        [ "bin" ]
                    }
                  ⫽ common.copts
                    [ "-fPIC"
                    , "-shared"
                    , "-no-hs-main"
                    , "-dynamic"
                    ]
            , name =
                "nrm.so"
            }
          , { executable =
                  λ(config : types.Config)
                →   prelude.defaults.Executable
                  ⫽ { main-is =
                        "bin/Codegen.hs"
                    , build-depends =
                        [ common.nobound "nrmlib" ]
                    }
                  ⫽ common.copts [ "-main-is", "Codegen" ]
            , name =
                "codegen"
            }
          , { executable =
                  λ(config : types.Config)
                →   prelude.defaults.Executable
                  ⫽ { main-is =
                        "bin/Hnrm.hs"
                    , build-depends =
                        [ common.nobound "nrmlib" ]
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
