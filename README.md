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
JSON schemas (raw) and Dhall types (documented with comments, for now)
are available.

when using JSON/YAML,  
- JSON schemas: `resources/schemas/{nrmd,manifest}.json`
- default values: `resources/defauts/{nrmd,manifest}.json`

when using Dhall,  
- types: `hsnrm/hsnrm/dhall/types/{nrmd,manifest}.dhall`
- default values: `hsnrm/hsnrm/dhall/defaults/{nrmd,manifest}.dhall`

Example `nrmd` configurations and `nrm` manifests are available in the
`examples` folder in JSON,YAML and Dhall format.

### APIs

JSON schema files for upstream and downstream APIs are available.
- JSON/YAML schemas: `resources/schemas/{upstream-*,downstream}.json`

## Development

- building: `make`
- running CI jobs locally: `make ci-<jobname>` or all jobs using `make ci`
  (requires gitlab-runner)
- entering a development shell : `nix-shell`. The appropriate `shellHooks` are
  in place for the `nrm` and `nrmd` aliases to be used on the CLI. 
