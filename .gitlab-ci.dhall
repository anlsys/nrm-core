let tags = [ "nix", "kvm" ]

let baseJob =
        λ(stage : Text)
      → λ(script : Text)
      → { stage = stage, tags = tags, script = script }

let mkJob =
        λ(stage : Text)
      → λ(target : Text)
      → baseJob stage ("nix-shell -p gnumake --run 'make " ++ target ++"'")

let mkS = mkJob "source"

let mkB = mkJob "build"

let mkNix =
        λ(stage : Text)
      → λ(target : Text)
      → baseJob "build" ("nix-build -A " ++ target ++ " --no-build-output")

let mkNixT = mkNix "test"

let mkNixB = mkNix "build"

in  { stages =
        [ "source", "build", "test", "deploy" ]
    , nix-hsnrm =
        mkNixB "haskellPackages.hsnrm"
    , nix-lib =
        mkNixB "libnrm"
    , nix-stream =
        mkNixB "stream"
    , dhrun-hello =
        mkNixT "dhrun-hello"
    , dhrun-exitcode =
        mkNixT "dhrun-exitcode"
    , dhrun-listen =
        mkNixT "dhrun-listen"
    , make-libnrm =
        mkB "libnrm"
    , make-hsnrm =
        mkB "hsnrm"
    , shellcheck =
        mkS "shellcheck"
    , dhall-format =
        mkS "dhall-format"
    , libnrm/clang-format =
        mkS "libnrm-clang-format"
    , pynrm-black =
        mkS "pynrm-black"
    , hsnrm-ormolu =
        mkS "hsnrm-ormolu"
    , hsnrm-hlint =
        mkS "hsnrm-hlint"
    , hsnrm-shellcheck =
        mkS "hsnrm-shellcheck"
    , hsnrm-dhall-format =
        mkS "hsnrm-dhall-format"
    , readthedocs =
        { stage =
            "deploy"
        , tags =
            tags
        , only =
            [ "master" ]
        , script =
            [ "echo \"token=\$RTD_TOKEN\""
            , "nix run nixpkgs.curl -c curl --fail -X POST -d \"token=\$RTD_TOKEN\" readthedocs.org/api/v2/webhook/hnrm/104604/"
            ]
        }
    }
