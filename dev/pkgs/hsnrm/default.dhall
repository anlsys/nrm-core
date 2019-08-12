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
      , types.Extension.MultiWayIf True
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
      , optparse-applicative =
          nobound "optparse-applicative"
      , hxt =
          nobound "hxt"
      , hxt-xpath =
          nobound "hxt-xpath"
      , refined =
          nobound "refined"
      , filepath =
          nobound "filepath"
      , neat-interpolation =
          nobound "neat-interpolation"
      , yaml =
          nobound "yaml"
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
      , data-default =
          nobound "data-default"
      , flat =
          nobound "flat"
      , unix =
          nobound "unix"
      , prettyprinter =
          nobound "prettyprinter"
      , containers =
          nobound "containers"
      , unordered-containers =
          nobound "unordered-containers"
      , zeromq4-conduit =
          nobound "zeromq4-conduit"
      , zeromq4-haskell =
          nobound "zeromq4-haskell"
      , uuid =
          nobound "uuid"
      , editor-open =
          nobound "editor-open"
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
      , vcs-revision =
          nobound "vcs-revision"
      , resourcet =
          nobound "resourcet"
      , mtl =
          nobound "mtl"
      , conduit =
          nobound "conduit"
      , conduit-extra =
          nobound "conduit-extra"
      , aeson-pretty =
          nobound "aeson-pretty"
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
      [ "Nrm.Types.Topology"
      , "Nrm.Types.Units"
      , "Nrm.Types.Container"
      , "Nrm.Types.Application"
      , "Nrm.Types.NrmState"
      , "Nrm.Types.Configuration"
      , "Nrm.Types.Client"
      , "Nrm.Types.Manifest"
      , "Nrm.Types.Messaging.DownstreamEvent"
      , "Nrm.Types.Messaging.UpstreamPub"
      , "Nrm.Types.Messaging.UpstreamReq"
      , "Nrm.Types.Messaging.UpstreamRep"
      , "Nrm.Types.Messaging.Protocols"
      , "Nrm.Containers"
      , "Nrm.Containers.Class"
      , "Nrm.Containers.Nodeos"
      , "Nrm.Containers.Singularity"
      , "Nrm.Containers.Dummy"
      , "Nrm.Node.Hwloc"
      , "Nrm.Node.Sysfs"
      , "Nrm.Control"
      , "Nrm.Codegen"
      , "Nrm.Export"
      , "Nrm.Client"
      , "Nrm.Daemon"
      , "Nrm.Optparse"
      , "Nrm.Behavior"
      , "Nrm.Version"
      , "Nrm.Node.Sysfs.Internal"
      , "Nrm.Optparse.Client"
      , "Nrm.Optparse.Daemon"
      , "Nrm.Types.Configuration.Dhall"
      , "Nrm.Types.Configuration.Yaml"
      , "Nrm.Types.Manifest.Dhall"
      , "Nrm.Types.Manifest.Yaml"
      ]

let extramodules =
      [ "FFI.TypeUncurry"
      , "FFI.TypeUncurry.Msgpack"
      , "FFI.TypeUncurry.DataKinds"
      , "Codegen.Schema"
      , "Codegen.Dhall"
      , "Codegen.CHeader"
      ]

let allmodules = modules # extramodules

let libdep =
      [ deps.base
      , deps.protolude
      , deps.vcs-revision
      , deps.transformers
      , deps.bytestring
      , deps.prettyprinter
      , deps.zeromq4-haskell
      , deps.zeromq4-conduit
      , deps.data-msgpack
      , deps.containers
      , deps.unordered-containers
      , deps.mtl
      , deps.aeson
      , deps.dhall
      , deps.conduit
      , deps.conduit-extra
      , deps.resourcet
      , deps.neat-interpolation
      , deps.generic-aeson
      , deps.aeson-pretty
      , deps.pretty-simple
      , deps.typed-process
      , deps.hxt
      , deps.hxt-xpath
      , deps.json-schema
      , deps.yaml
      , deps.data-default
      , deps.flat
      , deps.refined
      , deps.vector
      , deps.optparse-applicative
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
      , deps.filepath
      , deps.editor-open
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
          "The Node Resource Manager(NRM) is a linux daemon that enables dynamic resource optimization for improving the power/performance tradeoff of HPC applications."
      , sub-libraries =
          [ { library =
                  λ(config : types.Config)
                →   prelude.defaults.Library
                  ⫽ { build-depends =
                        libdep
                    , hs-source-dirs =
                        [ "nrm", "bin" ]
                    , exposed-modules =
                        allmodules
                    }
                  ⫽ copts ([] : List Text)
            , name =
                "nrmlib"
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
                        allmodules
                    }
                  ⫽ copts [ "-fPIC", "-shared", "-dynamic", "-no-hs-main" ]
            , name =
                "nrm.so"
            }
          , { executable =
                  λ(config : types.Config)
                →   prelude.defaults.Executable
                  ⫽ { main-is =
                        "Hnrm.hs"
                    , build-depends =
                        libdep
                    , hs-source-dirs =
                        [ "bin", "nrm" ]
                    , other-modules =
                        allmodules
                    }
                  ⫽ copts [ "-main-is", "Hnrm" ]
            , name =
                "nrm"
            }
          , { executable =
                  λ(config : types.Config)
                →   prelude.defaults.Executable
                  ⫽ { main-is =
                        "Hnrmd.hs"
                    , build-depends =
                        libdep
                    , hs-source-dirs =
                        [ "bin", "nrm" ]
                    , other-modules =
                        allmodules
                    }
                  ⫽ copts [ "-main-is", "Hnrmd" ]
            , name =
                "nrmd"
            }
          , { executable =
                  λ(config : types.Config)
                →   prelude.defaults.Executable
                  ⫽ { main-is =
                        "bin/Hnrm.hs"
                    , build-depends =
                        [ nobound "nrmlib" ]
                    }
                  ⫽ copts [ "-main-is", "Hnrm" ]
            , name =
                "nrmdep"
            }
          , { executable =
                  λ(config : types.Config)
                →   prelude.defaults.Executable
                  ⫽ { main-is =
                        "bin/Hnrmd.hs"
                    , build-depends =
                        [ nobound "nrmlib" ]
                    }
                  ⫽ copts [ "-main-is", "Hnrmd" ]
            , name =
                "nrmddep"
            }
          , { executable =
                  λ(config : types.Config)
                →   prelude.defaults.Executable
                  ⫽ { main-is =
                        "bin/Codegen.hs"
                    , build-depends =
                        [ nobound "nrmlib" ]
                    }
                  ⫽ copts [ "-main-is", "Codegen" ]
            , name =
                "codegen"
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
