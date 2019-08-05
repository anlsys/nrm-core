  --λ(ghcVersion : Text)
let prelude = ../../dhall-to-cabal/prelude.dhall

let types = ../../dhall-to-cabal/types.dhall

let defexts =
      [ types.Extension.LambdaCase True
      , types.Extension.QuasiQuotes True
      , types.Extension.DefaultSignatures True
      , types.Extension.RecordWildCards True
      , types.Extension.TypeSynonymInstances True
      , types.Extension.StandaloneDeriving True
      , types.Extension.FlexibleInstances True
      , types.Extension.TupleSections True
      , types.Extension.MultiParamTypeClasses True
      , types.Extension.ImplicitPrelude False
      , types.Extension.OverloadedStrings True
      , types.Extension.ViewPatterns True
      , types.Extension.DeriveAnyClass True
      , types.Extension.DeriveGeneric True
      , types.Extension.TemplateHaskell True
      , types.Extension.BlockArguments True
      , types.Extension.GADTs True
      , types.Extension.FlexibleContexts True
      , types.Extension.TypeOperators True
      , types.Extension.DataKinds True
      , types.Extension.PolyKinds True
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
      , aeson =
          nobound "aeson"
      , hsnrm-lib =
          nobound "hsnrm-lib"
      , directory =
          nobound "directory"
      , transformers =
          nobound "transformers"
      , regex =
          nobound "regex"
      , units-defs =
          nobound "units-defs"
      , units =
          nobound "units"
      , unix =
          nobound "unix"
      , containers =
          nobound "containers"
      , unordered-containers =
          nobound "unordered-containers"
      , uuid =
          nobound "uuid"
      , text =
          nobound "text"
      , dhall =
          nobound "dhall"
      , bytestring =
          nobound "bytestring"
      , data-msgpack =
          nobound "data-msgpack"
      , storable-endian =
          nobound "storable-endian"
      , template-haskell =
          nobound "template-haskell"
      , mtl =
          nobound "mtl"
      , generic-aeson =
          nobound "generic-aeson"
      , vector =
          nobound "vector"
      , json-schema =
          nobound "json-schema"
      , ffi-nh2 =
          nobound "ffi-nh2"
      }

let modules =
      [ "Nrm.Node.Hwloc"
      , "Nrm.Node.Sysfs"
      , "Nrm.Node.Internal.Sysfs"
      , "Nrm.Types.Topo"
      , "Nrm.Types.Containers"
      , "Nrm.Types.Applications"
      , "Nrm.Control"
      , "Nrm.Configuration"
      , "Nrm.Messaging.Downstream"
      , "Nrm.Messaging.UpstreamPub"
      , "Nrm.Messaging.UpstreamReq"
      , "Nrm.Messaging.UpstreamRep"
      , "Nrm.Containers"
      , "Nrm.Containers.Class"
      , "Nrm.Containers.Nodeos"
      , "Nrm.Containers.Singularity"
      , "Nrm.Containers.Dummy"
      ]

let libdep =
      [ deps.base
      , deps.protolude
      , deps.transformers
      , deps.bytestring
      , deps.data-msgpack
      , deps.containers
      , deps.unordered-containers
      , deps.mtl
      , deps.aeson
      , deps.dhall
      , deps.generic-aeson
      , deps.pretty-simple
      , deps.typed-process
      , deps.hxt
      , deps.hxt-xpath
      , deps.json-schema
      , deps.refined
      , deps.vector
      , deps.directory
      , deps.regex
      , deps.units
      , deps.unix
      , deps.uuid
      , deps.text
      , deps.units-defs
      , deps.storable-endian
      , deps.template-haskell
      , deps.mtl
      ]

in    prelude.defaults.Package
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
          "haskell node resource manager prototype"
      , sub-libraries =
          [ { library =
                  λ(config : types.Config)
                →   prelude.defaults.Library
                  ⫽ { build-depends =
                        libdep
                    , hs-source-dirs =
                        [ "nrm" ]
                    , exposed-modules =
                        modules
                    }
                  ⫽ copts ([] : List Text)
            , name =
                "hsnrm-lib"
            }
          , { library =
                  λ(config : types.Config)
                →   prelude.defaults.Library
                  ⫽ { build-depends =
                        libdep
                    , hs-source-dirs =
                        [ "nrm", "bin" ]
                    , exposed-modules =
                          modules
                        # [ "Hnrm", "Hnrmd", "Codegen" ]
                        # [ "FFI.TH"
                          , "FFI.TypeUncurry"
                          , "FFI.TypeUncurry.Msgpack"
                          , "FFI.TypeUncurry.DataKinds"
                          , "Codegen.Schema"
                          , "Codegen.CHeader"
                          ]
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
                        "Export.hs"
                    , build-depends =
                        libdep
                    , hs-source-dirs =
                        [ "bin", "nrm" ]
                    , other-modules =
                          modules
                        # [ "Codegen"
                          , "Hnrm"
                          , "Hnrmd"
                          , "FFI.TH"
                          , "FFI.TypeUncurry"
                          , "FFI.TypeUncurry.Msgpack"
                          , "FFI.TypeUncurry.DataKinds"
                          , "Codegen.Schema"
                          , "Codegen.CHeader"
                          ]
                    }
                  ⫽ copts [ "-fPIC", "-shared", "-dynamic", "-no-hs-main" ]
            , name =
                "hsnrm.so"
            }
          , { executable =
                  λ(config : types.Config)
                →   prelude.defaults.Executable
                  ⫽ { main-is =
                        "Hnrm.hs"
                    , build-depends =
                        [ deps.base, deps.protolude, deps.hsnrm-lib ]
                    , hs-source-dirs =
                        [ "bin" ]
                    }
                  ⫽ copts [ "-threaded", "-main-is", "Hnrm" ]
            , name =
                "nrm"
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
                  Some "https://xgitlab.cels.anl.gov/vreis/hsnrm.git"
              }
          ]
      , synopsis =
          "hsnrm"
      }
