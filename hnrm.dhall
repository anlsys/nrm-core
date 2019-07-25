let prelude =
      https://raw.githubusercontent.com/dhall-lang/dhall-to-cabal/1.3.4.0/dhall/prelude.dhall

let types =
      https://raw.githubusercontent.com/dhall-lang/dhall-to-cabal/1.3.4.0/dhall/types.dhall

let defexts =
      [ types.Extension.LambdaCase True
      , types.Extension.RecordWildCards True
      , types.Extension.ScopedTypeVariables True
      , types.Extension.ImplicitPrelude False
      , types.Extension.OverloadedStrings True
      , types.Extension.ViewPatterns True
      ]

let defcopts =
        [ "-threaded"
        , "-Wall"
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
      : List Text

let deflang = Some types.Language.Haskell2010

let map = https://prelude.dhall-lang.org/List/map

let nobound = λ(p : Text) → { bounds = prelude.anyVersion, package = p }

let nobounds =
        λ(ps : List Text)
      → map Text { bounds : types.VersionRange, package : Text } nobound ps

let common =
      { compiler-options =
          prelude.defaults.CompilerOptions ⫽ { GHC = defcopts }
      , default-extensions =
          defexts
      , default-language =
          deflang
      }

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
      , executables =
          [ { executable =
                  λ(config : types.Config)
                →   prelude.defaults.Executable
                  ⫽ { main-is =
                        "hnrm.hs"
                    , build-depends =
                        nobounds [ "base", "protolude", "typed-process" ]
                    , hs-source-dirs =
                        [ "src" ]
                    }
                  ⫽ common
            , name =
                "hnrm"
            }
          , { executable =
                  λ(config : types.Config)
                →   prelude.defaults.Executable
                  ⫽ { main-is =
                        "hnrmd.hs"
                    , build-depends =
                        nobounds
                        [ "base"
                        , "pretty-simple"
                        , "protolude"
                        , "typed-process"
                        , "hxt"
                        , "hxt-xpath"
                        , "refined"
                        ]
                    , hs-source-dirs =
                        [ "src" ]
                    }
                  ⫽ common
            , name =
                "hnrmd"
            }
          ]
      , extra-source-files =
          [ "ChangeLog.md" ]
      , license =
          types.License.MIT
      , license-files =
          [ "LICENSE" ]
      , maintainer =
          "fre@freux.fr"
      , source-repos =
          [   prelude.defaults.SourceRepo
            ⫽ { type =
                  Some types.RepoType.Git
              , location =
                  Some "https://github.com/freuk/hnrm"
              }
          ]
      , synopsis =
          "hnrm configuration"
      }
