let prelude = ./dhall/dhall-to-cabal/prelude.dhall

let types = ./dhall/dhall-to-cabal/types.dhall

let common = ./dhall/common.dhall

in \(ghcPath:Text)  -> \(ghcNumericVersion:Text) ->   prelude.defaults.Package
    ⫽ { name =
          "nrmlib"
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
      ,library = Some (
                  λ(config : types.Config)
                →   prelude.defaults.Library
                  ⫽ { build-depends =
                        common.libdep
                    , hs-source-dirs =
                        [ "nrm" ]
                    , exposed-modules =
                        common.allmodules
                    }
                  ⫽ common.copts ([] : List Text))
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
