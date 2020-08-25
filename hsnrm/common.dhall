let prelude = ./dhall-to-cabal/prelude.dhall

let types = ./dhall-to-cabal/types.dhall

let defexts =
      [ types.Extension.LambdaCase True
      , types.Extension.QuasiQuotes True
      , types.Extension.DefaultSignatures True
      , types.Extension.OverloadedLists True
      , types.Extension.ExistentialQuantification True
      , types.Extension.RecordWildCards True
      , types.Extension.RankNTypes True
      , types.Extension.TypeSynonymInstances True
      , types.Extension.StandaloneDeriving True
      , types.Extension.FlexibleInstances True
      , types.Extension.TupleSections True
      , types.Extension.MultiParamTypeClasses True
      , types.Extension.ImplicitPrelude False
      , types.Extension.OverloadedStrings True
      , types.Extension.ViewPatterns True
      , types.Extension.PatternSynonyms True
      , types.Extension.DeriveFunctor True
      , types.Extension.DeriveTraversable True
      , types.Extension.TypeFamilies True
      , types.Extension.DeriveAnyClass True
      , types.Extension.DeriveGeneric True
      , types.Extension.DeriveDataTypeable True
      , types.Extension.OverloadedLabels True
      , types.Extension.DeriveFoldable True
      , types.Extension.DerivingStrategies True
      , types.Extension.TypeApplications True
      , types.Extension.MultiWayIf True
      , types.Extension.TemplateHaskell False
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
      → { compiler-options = defcopts addcopts
        , default-extensions = defexts
        , default-language = deflang
        }

let nobound = λ(p : Text) → { bounds = prelude.anyVersion, package = p }

let deps =
      { base = nobound "base"
      , hbandit = nobound "hbandit"
      , iso-deriving = nobound "iso-deriving"
      , pretty-simple = nobound "pretty-simple"
      , protolude = nobound "protolude"
      , Chart = nobound "Chart"
      , Chart-cairo = nobound "Chart-cairo"
      , gtk3 = nobound "gtk3"
      , cereal = nobound "cereal"
      , tree-view = nobound "tree-view"
      , dynamic-graph = nobound "dynamic-graph"
      , colour = nobound "colour"
      , transformers = nobound "transformers"
      , glib = nobound "glib"
      , opengl = nobound "OpenGL"
      , scientific = nobound "scientific"
      , generic-deriving = nobound "generic-deriving"
      , typed-process = nobound "typed-process"
      , optparse-applicative = nobound "optparse-applicative"
      , aeson-pretty = nobound "aeson-pretty"
      , random = nobound "random"
      , pipes = nobound "pipes"
      , either = nobound "either"
      , transformers-either = nobound "transformers-either"
      , intervals = nobound "intervals"
      , hxt = nobound "hxt"
      , hxt-xpath = nobound "hxt-xpath"
      , recursion-schemes = nobound "recursion-schemes"
      , binary = nobound "binary"
      , refined = nobound "refined"
      , generic-data = nobound "generic-data"
      , filepath = nobound "filepath"
      , neat-interpolation = nobound "neat-interpolation"
      , generic-lens = nobound "generic-lens"
      , yaml = nobound "yaml"
      , plot-ho-matic = nobound "Plot-ho-matic"
      , aeson = nobound "aeson"
      , hsnrm-lib = nobound "hsnrm-lib"
      , monadRandom = nobound "MonadRandom"
      , directory = nobound "directory"
      , regex = nobound "regex"
      , units-defs = nobound "units-defs"
      , mtl-compat = nobound "mtl-compat"
      , units = nobound "units"
      , data-default = nobound "data-default"
      , flat = nobound "flat"
      , unix = nobound "unix"
      , prettyprinter = nobound "prettyprinter"
      , containers = nobound "containers"
      , unordered-containers = nobound "unordered-containers"
      , zeromq4-conduit = nobound "zeromq4-conduit"
      , zeromq4-haskell = nobound "zeromq4-haskell"
      , uuid = nobound "uuid"
      , text = nobound "text"
      , dhall = nobound "dhall"
      , bytestring = nobound "bytestring"
      , data-msgpack = nobound "data-msgpack"
      , enclosed-exceptions = nobound "enclosed-exceptions"
      , storable-endian = nobound "storable-endian"
      , template-haskell = nobound "template-haskell"
      , vcs-revision = nobound "vcs-revision"
      , resourcet = nobound "resourcet"
      , async = nobound "async"
      , dhall-json = nobound "dhall-json"
      , lens = nobound "lens"
      , mtl = nobound "mtl"
      , conduit = nobound "conduit"
      , brick = nobound "brick"
      , aeson-extra = nobound "aeson-extra"
      , conduit-extra = nobound "conduit-extra"
      , generic-aeson = nobound "generic-aeson"
      , vector = nobound "vector"
      , json-schema = nobound "json-schema"
      , megaparsec = nobound "megaparsec"
      }

let modules =
      [ "NRM.Slices"
      , "NRM.Slices.Class"
      , "NRM.Slices.Nodeos"
      , "NRM.Slices.Singularity"
      , "NRM.Slices.Dummy"
      , "NRM.Node.Hwloc"
      , "NRM.Node.Sysfs"
      , "NRM.Control"
      , "NRM.Codegen"
      , "NRM.Export"
      , "NRM.ExportIO"
      , "NRM.Client"
      , "NRM.Daemon"
      , "NRM.CPD"
      , "NRM.Optparse"
      , "NRM.Behavior"
      , "NRM.Version"
      , "NRM.Sensors"
      , "NRM.Actuators"
      , "NRM.Messaging"
      , "NRM.Processes"
      , "NRM.State"
      , "NRM.Node.Sysfs.Internal"
      , "NRM.Optparse.Client"
      , "NRM.Optparse.Daemon"
      , "NRM.Types.Topology"
      , "NRM.Types.Topology.Package"
      , "NRM.Types.Topology.Core"
      , "NRM.Types.Topology.PU"
      , "NRM.Types.Topology.PackageID"
      , "NRM.Types.Topology.CoreID"
      , "NRM.Types.Topology.PUID"
      , "NRM.Types.Units"
      , "NRM.Types.Controller"
      , "NRM.Types.Actuator"
      , "NRM.Types.Sensor"
      , "NRM.Types.Behavior"
      , "NRM.Types.Slice"
      , "NRM.Types.Cmd"
      , "NRM.Types.CmdID"
      , "NRM.Types.NRM"
      , "NRM.Types.Process"
      , "NRM.Types.DownstreamThread"
      , "NRM.Types.DownstreamThreadID"
      , "NRM.Types.DownstreamCmd"
      , "NRM.Types.DownstreamCmdID"
      , "NRM.Types.State"
      , "NRM.Types.MemBuffer"
      , "NRM.Types.UpstreamClient"
      , "NRM.Types.DownstreamClient"
      , "NRM.Classes.Messaging"
      , "NRM.Classes.Sensors"
      , "NRM.Classes.Actuators"
      , "NRM.Classes.Topology"
      , "NRM.Orphans.ExitCode"
      , "NRM.Orphans.UUID"
      , "NRM.Orphans.Dhall"
      , "NRM.Orphans.NonEmpty"
      , "NRM.Orphans.ZeroOne"
      , "NRM.Orphans.Refined"
      , "NRM.Types.Messaging.DownstreamEvent"
      , "NRM.Types.Messaging.UpstreamPub"
      , "NRM.Types.Messaging.UpstreamReq"
      , "NRM.Types.Messaging.UpstreamRep"
      , "NRM.Types.Messaging.Protocols"
      , "NRM.Types.Configuration"
      , "NRM.Types.Manifest"
      ]

let extramodules =
      [ "FFI.TypeUncurry"
      , "FFI.TypeUncurry.Msgpack"
      , "FFI.TypeUncurry.DataKinds"
      , "Codegen.Schema"
      , "Codegen.Dhall"
      , "Codegen.CHeader"
      , "LensMap.Core"
      , "CPD.Core"
      , "CPD.Integrated"
      , "CPD.Values"
      , "CPD.Utils"
      , "CPD.Text"
      ]

let allmodules = modules # extramodules

let libdep =
      [ deps.base
      , deps.hbandit
      , deps.either
      , deps.protolude
      , deps.scientific
      , deps.megaparsec
      , deps.enclosed-exceptions
      , deps.monadRandom
      , deps.mtl-compat
      , deps.vcs-revision
      , deps.transformers
      , deps.generic-data
      , deps.bytestring
      , deps.iso-deriving
      , deps.random
      , deps.prettyprinter
      , deps.zeromq4-haskell
      , deps.zeromq4-conduit
      , deps.generic-lens
      , deps.data-msgpack
      , deps.containers
      , deps.binary
      , deps.unordered-containers
      , deps.mtl
      , deps.aeson
      , deps.dhall
      , deps.dhall-json
      , deps.conduit
      , deps.conduit-extra
      , deps.aeson-extra
      , deps.resourcet
      , deps.intervals
      , deps.neat-interpolation
      , deps.generic-aeson
      , deps.aeson-pretty
      , deps.pretty-simple
      , deps.typed-process
      , deps.hxt
      , deps.recursion-schemes
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
      , deps.generic-deriving
      , deps.uuid
      , deps.async
      , deps.text
      , deps.units-defs
      , deps.storable-endian
      , deps.template-haskell
      , deps.mtl
      , deps.brick
      , deps.filepath
      , deps.lens
      ]

in  { defexts = defexts
    , deps = deps
    , libdep = libdep
    , allmodules = allmodules
    , copts = copts
    , nobound = nobound
    }
