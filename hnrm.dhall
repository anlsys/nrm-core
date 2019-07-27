let prelude = ./cabal/prelude.dhall

let types = ./cabal/types.dhall

let defexts =
      [ types.Extension.LambdaCase True
      , types.Extension.RecordWildCards True
      , types.Extension.TypeSynonymInstances True
      , types.Extension.StandaloneDeriving True
      , types.Extension.DataKinds True
      , types.Extension.FlexibleInstances True
      , types.Extension.TupleSections True
      , types.Extension.MultiParamTypeClasses True
      , types.Extension.ScopedTypeVariables True
      , types.Extension.ImplicitPrelude False
      , types.Extension.OverloadedStrings True
      , types.Extension.ViewPatterns True
      ]

let deflang = Some types.Language.Haskell2010

let defcopts =
        λ(addcopts : List Text)
      →   prelude.defaults.CompilerOptions
        ⫽ { GHC =
                  [ "-Wall"
                  , "-Wcompat"
                  , "-Wincomplete-uni-patterns"
                  , "-Wincomplete-record-updates"
                  , "-Wmissing-home-modules"
                  , "-Widentities"
                  , "-Wredundant-constraints"
                  , "-Wcpp-undef"
                  , "-fwarn-tabs"
                  , "-fwarn-unused-imports"
                  , "-fwarn-missing-signatures"
                  , "-fwarn-name-shadowing"
                  , "-fprint-potential-instances"
                  , "-Wmissing-export-lists"
                  , "-fwarn-unused-do-bind"
                  , "-fwarn-wrong-do-bind"
                  , "-fwarn-incomplete-patterns"
                  ]
                # addcopts
              : List Text
          }

let copts =
        λ(addcopts : List Text)
      → { compiler-options =
            defcopts addcopts
        , default-extensions =
            defexts
        , default-language =
            deflang
        }

let nobound = λ(p : Text) → { bounds = prelude.anyVersion, package = p }

let deps =
      { base =
          nobound "base"
      , pretty-simple =
          nobound "pretty-simple"
      , protolude =
          nobound "protolude"
      , typed-process =
          nobound "typed-process"
      , hxt =
          nobound "hxt"
      , hxt-xpath =
          nobound "hxt-xpath"
      , refined =
          nobound "refined"
      , hnrm-lib =
          nobound "hnrm-lib"
      , directory =
          nobound "directory"
      , regex =
          nobound "regex"
      , units-defs =
          nobound "units-defs"
      , units =
          nobound "units"
      }

let modules =
      [ "Nrm.Node"
      , "Nrm.Node.Hwloc"
      , "Nrm.Node.Sysfs"
      , "Nrm.Types"
      , "Nrm.Types.Topo"
      , "Nrm.Types.Units"
      ]

let libdep =
      [ deps.base
      , deps.pretty-simple
      , deps.protolude
      , deps.typed-process
      , deps.hxt
      , deps.hxt-xpath
      , deps.refined
      , deps.directory
      , deps.regex
      , deps.units
      , deps.units-defs
      ]

in    prelude.defaults.Package
    ⫽ { name =
          "hnrm"
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
          "haskell node resource manager prototype"
      , sub-libraries =
          [ { library =
                  λ(config : types.Config)
                →   prelude.defaults.Library
                  ⫽ { build-depends =
                        libdep
                    , hs-source-dirs =
                        [ "src" ]
                    , exposed-modules =
                        modules
                    }
                  ⫽ copts ([] : List Text)
            , name =
                "hnrm-lib"
            }
          , { library =
                  λ(config : types.Config)
                →   prelude.defaults.Library
                  ⫽ { build-depends =
                        libdep
                    , hs-source-dirs =
                        [ "src", "app" ]
                    , exposed-modules =
                        modules # [ "Hnrmd", "Hnrm" ]
                    }
                  ⫽ copts ([] : List Text)
            , name =
                "monolith"
            }
          ]
      , executables =
          [ { executable =
                  λ(config : types.Config)
                →   prelude.defaults.Executable
                  ⫽ { main-is =
                        "Hnrm.hs"
                    , build-depends =
                        [ deps.base, deps.protolude, deps.hnrm-lib ]
                    , hs-source-dirs =
                        [ "app" ]
                    }
                  ⫽ copts [ "-threaded", "-main-is", "Hnrm" ]
            , name =
                "hnrm"
            }
          , { executable =
                  λ(config : types.Config)
                →   prelude.defaults.Executable
                  ⫽ { main-is =
                        "Hnrmd.hs"
                    , build-depends =
                        [ deps.base, deps.protolude, deps.hnrm-lib ]
                    , hs-source-dirs =
                        [ "app" ]
                    }
                  ⫽ copts [ "-threaded", "-main-is", "Hnrmd" ]
            , name =
                "hnrmd"
            }
          ]
      , extra-source-files =
          [ "ChangeLog.md" ]
      , license =
          types.License.BSD3
      , license-files =
          [ "LICENSE" ]
      , maintainer =
          "fre@freux.fr"
      , source-repos =
          [   prelude.defaults.SourceRepo
            ⫽ { type =
                  Some types.RepoType.Git
              , location =
                  Some "https://xgitlab.cels.anl.gov/vreis/hnrm.git"
              }
          ]
      , synopsis =
          "hnrm configuration"
      }
