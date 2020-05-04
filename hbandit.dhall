let prelude = ./dhall-to-cabal/prelude.dhall

let types = ./dhall-to-cabal/types.dhall

let defexts =
      [ types.Extension.LambdaCase True
      , types.Extension.QuasiQuotes True
      , types.Extension.DefaultSignatures True
      , types.Extension.ExistentialQuantification True
      , types.Extension.RecordWildCards True
      , types.Extension.TypeSynonymInstances True
      , types.Extension.StandaloneDeriving True
      , types.Extension.FlexibleInstances True
      , types.Extension.TupleSections True
      , types.Extension.MultiParamTypeClasses True
      , types.Extension.ImplicitPrelude False
      , types.Extension.OverloadedStrings True
      , types.Extension.OverloadedLists True
      , types.Extension.ViewPatterns True
      , types.Extension.OverloadedLabels True
      , types.Extension.DeriveFunctor True
      , types.Extension.TypeFamilies True
      , types.Extension.DeriveAnyClass True
      , types.Extension.DeriveGeneric True
      , types.Extension.DeriveDataTypeable True
      , types.Extension.DeriveFoldable True
      , types.Extension.DerivingStrategies True
      , types.Extension.TypeApplications True
      , types.Extension.MultiWayIf True
      , types.Extension.TemplateHaskell True
      , types.Extension.BlockArguments True
      , types.Extension.GADTs True
      , types.Extension.FlexibleContexts True
      , types.Extension.TypeOperators True
      , types.Extension.DataKinds True
      , types.Extension.PolyKinds True
      , types.Extension.AllowAmbiguousTypes True
      , types.Extension.FunctionalDependencies True
      , types.Extension.UndecidableInstances True
      ]

let deflang = Some types.Language.Haskell2010

let defcopts =
        λ(addcopts : List Text)
      →   prelude.defaults.CompilerOptions
        ⫽ { GHC =
                  [ "-Wall"
                  , "-O0"
                  , "-Wcompat"
                  , "-Wincomplete-uni-patterns"
                  , "-Widentities"
                  , "-Wredundant-constraints"
                  , "-Wcpp-undef"
                  , "-fwarn-tabs"
                  , "-fwarn-unused-imports"
                  , "-fwarn-missing-signatures"
                  , "-fwarn-name-shadowing"
                  , "-fprint-potential-instances"
                  , "-fwarn-unused-do-bind"
                  , "-fwarn-wrong-do-bind"
                  , "-fwarn-incomplete-patterns"
                  , "-Wincomplete-record-updates"
                  , "-Wmonomorphism-restriction"
                  , "-Wimplicit-prelude"
                  , "-Wmissing-local-signatures"
                  , "-Wmissing-exported-signatures"
                  , "-Wmissing-export-lists"
                  , "-Wmissing-home-modules"
                  , "-Widentities"
                  , "-Wredundant-constraints"
                  , "-Wpartial-fields"
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
      , protolude =
          nobound "protolude"
      , random =
          nobound "random"
      , refined =
          nobound "refined"
      , intervals =
          nobound "intervals"
      , primitive =
          nobound "primitive"
      , containers =
          nobound "containers"
      , bytestring =
          nobound "bytestring"
      , storable-endian =
          nobound "storable-endian"
      , monadRandom =
          nobound "MonadRandom"
      , lens =
          nobound "lens"
      , generic-lens =
          nobound "generic-lens"
      , enclosed-exceptions =
          nobound "enclosed-exceptions"
      }

let allmodules =
      [ "Bandit"
      , "Bandit.Class"
      , "Bandit.EpsGreedy"
      , "Bandit.Exp3"
      , "Bandit.Exp4R"
      , "Bandit.Types"
      , "Bandit.Tutorial"
      , "Bandit.Util"
      ]

let libdep =
      [ deps.base
      , deps.protolude
      , deps.random
      , deps.refined
      , deps.intervals
      , deps.monadRandom
      , deps.lens
      , deps.generic-lens
      ]

let common =
      { defexts =
          defexts
      , deps =
          deps
      , libdep =
          libdep
      , allmodules =
          allmodules
      , copts =
          copts
      , nobound =
          nobound
      }

in    prelude.defaults.Package
    ⫽ { name =
          "hbandit"
      , version =
          prelude.v "1.0.0"
      , author =
          "Name Lastname"
      , build-type =
          Some types.BuildType.Simple
      , cabal-version =
          prelude.v "2.0"
      , category =
          "algorithms"
      , description =
          "hbandit"
      , library =
          prelude.unconditional.library
          (   prelude.defaults.Library
            ⫽ { build-depends =
                  common.libdep
              , hs-source-dirs =
                  [ "src" ]
              , exposed-modules =
                  common.allmodules
              }
            ⫽ common.copts ([] : List Text)
          )
      , test-suites =
          [ prelude.unconditional.test-suite
            "test"
            (   prelude.defaults.TestSuite
              ⫽ { build-depends =
                    common.libdep # [ nobound "hbandit" ]
                , hs-source-dirs =
                    [ "test" ]
                , type =
                    types.TestType.exitcode-stdio { main-is = "ctx.hs" }
                }
              ⫽ common.copts ([] : List Text)
            )
          ]
      , extra-doc-files =
          [ "literate/*.png" ] : List Text
      , extra-source-files =
          [] : List Text
      , license =
          types.License.BSD3
      , license-files =
          [] : List Text
      , maintainer =
          "fre@freux.fr"
      , source-repos =
          [ prelude.defaults.SourceRepo ]
      , synopsis =
          "hbandit"
      }
