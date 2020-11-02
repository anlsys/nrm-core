# NRM

## User Documentation

- [User documentation](http://hnrm.readthedocs.io)

## Installation

All the following instructions suppose using a local clone:

```
git clone https://xgitlab.cels.anl.gov/argo/hnrm.git
```

- local nix build: `nix-build -A nrm`
- local nix profile install: `nix-env -f. -iA nrm`

## Usage

### binaries

```
#daemon
nrmd --help
#client
nrm --help
```

### `nrmd` configuration / `nrm` manifests

`nrmd` configurations and `nrm` manifests can use any of YAML, JSON or Dhall.
These two configuration formats are defined and documented using Dhall types.
Default values and examples are available in JSON/YAML/Dhall format.

when using JSON/YAML, relevant files are:  
- default values: `resources/defauts/{nrmd,manifest}.json`
- examples: `examples/*/*.{json,yaml}`

when using Dhall, relevant files are:  
- types: `hsnrm/hsnrm/dhall/types/{nrmd,manifest}.dhall`
- default values: `hsnrm/hsnrm/dhall/defaults/{nrmd,manifest}.dhall`
- examples: `examples/*/*.dhall`

The authoritative source of documentation for both the configuration format and
manifest files is contained in Dhall type definitions:
`hsnrm/hsnrm/dhall/types/{nrmd,manifest}.dhall`.

### APIs

JSON schema files for upstream and downstream APIs are available:  
- JSON/YAML schemas: `resources/schemas/{upstream-*,downstream}.json`

### notebooks

Tutorial notebooks for configuration and upstream python bindings are available
in the `notebooks` folder.

## Development

- building: `make`
- running CI jobs locally: `make ci-<jobname>` or all jobs using `make ci`
  (requires gitlab-runner)
- entering a development shell : `nix-shell`. The appropriate `shellHooks` are
  in place for the `nrm` and `nrmd` aliases to be used on the CLI.
