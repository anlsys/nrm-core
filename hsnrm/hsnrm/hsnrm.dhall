let prelude = ../dhall-to-cabal/prelude.dhall

let types = ../dhall-to-cabal/types.dhall

let common = ../common.dhall

in    λ(ghcPath : Text)
    → λ(ghcNumericVersion : Text)
    →   prelude.defaults.Package
      ⫽ { name = "hsnrm"
        , version = prelude.v "1.0.0"
        , author = "Valentin Reis"
        , build-type = Some types.BuildType.Simple
        , cabal-version = prelude.v "2.0"
        , category = "tools"
        , description =
            "The Node Resource Manager(NRM) is a linux daemon that enables dynamic resource optimization for improving the power/performance tradeoff of HPC applications."
        , library =
            prelude.unconditional.library
              (   prelude.defaults.Library
                ⫽ { build-depends = common.libdep
                  , hs-source-dirs = [ "src" ]
                  , exposed-modules = common.allmodules
                  }
                ⫽ common.copts ([] : List Text)
              )
        , test-suites =
          [ { name = "discover"
            , test-suite =
                  λ(config : types.Config)
                →   prelude.defaults.TestSuite
                  ⫽ { type =
                        types.TestType.exitcode-stdio { main-is = "Driver.hs" }
                    , other-modules = [ "Test.CPD.Integrated" ]
                    , build-depends =
                      [ common.deps.base
                      , common.deps.protolude
                      , common.nobound "hsnrm"
                      , common.deps.tasty-hunit
                      , common.deps.tasty
                      , common.deps.tasty-discover
                      ]
                    , hs-source-dirs = [ "tests" ]
                    }
                  ⫽ common.copts [ "-threaded" ]
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
        , synopsis = "hsnrm"
        }
