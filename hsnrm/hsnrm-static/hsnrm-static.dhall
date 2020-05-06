let prelude = ../dhall-to-cabal/prelude.dhall

let types = ../dhall-to-cabal/types.dhall

let common = ../common.dhall

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
