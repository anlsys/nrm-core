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
      , types.Extension.ViewPatterns True
      , types.Extension.DeriveFunctor True
      , types.Extension.DeriveTraversable True
      , types.Extension.TypeFamilies True
      , types.Extension.DeriveAnyClass True
      , types.Extension.DeriveGeneric True
      , types.Extension.DeriveDataTypeable True
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
      , generic-deriving =
          nobound "generic-deriving"
      , typed-process =
          nobound "typed-process"
      , optparse-applicative =
          nobound "optparse-applicative"
      , random =
          nobound "random"
      , intervals =
          nobound "intervals"
      , hxt =
          nobound "hxt"
      , hxt-xpath =
          nobound "hxt-xpath"
      , recursion-schemes =
          nobound "recursion-schemes"
      , binary =
          nobound "binary"
      , refined =
          nobound "refined"
      , generic-data =
          nobound "generic-data"
      , filepath =
          nobound "filepath"
      , neat-interpolation =
          nobound "neat-interpolation"
      , generic-lens =
          nobound "generic-lens"
      , yaml =
          nobound "yaml"
      , aeson =
          nobound "aeson"
      , hsnrm-lib =
          nobound "hsnrm-lib"
      , monadRandom =
          nobound "MonadRandom"
      , directory =
          nobound "directory"
      , transformers =
          nobound "transformers"
      , regex =
          nobound "regex"
      , units-defs =
          nobound "units-defs"
      , mtl-compat =
          nobound "mtl-compat"
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
      , enclosed-exceptions =
          nobound "enclosed-exceptions"
      , storable-endian =
          nobound "storable-endian"
      , template-haskell =
          nobound "template-haskell"
      , vcs-revision =
          nobound "vcs-revision"
      , resourcet =
          nobound "resourcet"
      , async =
          nobound "async"
      , dhall-json =
          nobound "dhall-json"
      , lens =
          nobound "lens"
      , mtl =
          nobound "mtl"
      , conduit =
          nobound "conduit"
      , brick =
          nobound "brick"
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
      , "NRM.Classes.Examples"
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
      , "NRM.Configuration.Examples"
      , "NRM.Types.Manifest"
      , "NRM.Manifest.Examples"
      ]

let extramodules =
      [ "FFI.TypeUncurry"
      , "FFI.TypeUncurry.Msgpack"
      , "FFI.TypeUncurry.DataKinds"
      , "Codegen.Schema"
      , "Codegen.Dhall"
      , "Codegen.CHeader"
      , "LensMap.Core"
      , "LMap.Map"
      , "LMap.NonEmpty"
      , "CPD.Core"
      , "CPD.Integrated"
      , "CPD.Values"
      , "CPD.Utils"
      , "CPD.Text"
      , "Control.Monad.LPMonad.Internal"
      , "Control.Monad.LPMonad.Supply.Class"
      , "Control.Monad.LPMonad.Supply"
      , "Control.Monad.LPMonad"
      , "Data.LinearProgram"
      , "Data.LinearProgram.Common"
      , "Data.LinearProgram.GLPK"
      , "Data.LinearProgram.GLPK.Common"
      , "Data.LinearProgram.GLPK.Internal"
      , "Data.LinearProgram.GLPK.IO.Internal"
      , "Data.LinearProgram.GLPK.IO"
      , "Data.LinearProgram.GLPK.Solver"
      , "Data.LinearProgram.GLPK.Types"
      , "Data.LinearProgram.LinExpr"
      , "Data.LinearProgram.Spec"
      , "Data.LinearProgram.Types"
      , "Algebra.Classes"
      ]

let banditmodules =
      [ "HBandit.Class"
      , "HBandit.Exp3"
      , "HBandit.EpsGreedy"
      , "HBandit.Util"
      , "HBandit.Types"
      , "HBandit.BwCR"
      ]

let allmodules = modules # extramodules # banditmodules

let libdep =
      [ deps.base
      , deps.protolude
      , deps.enclosed-exceptions
      , deps.monadRandom
      , deps.mtl-compat
      , deps.vcs-revision
      , deps.transformers
      , deps.generic-data
      , deps.bytestring
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
      , deps.editor-open
      ]

in  { defexts =
        defexts
    , deps =
        deps
    , libdep =
        libdep
    , banditmodules =
        banditmodules
    , allmodules =
        allmodules
    , copts =
        copts
    , nobound =
        nobound
    }
