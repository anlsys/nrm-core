xmonad configuration

##### Hacking

-   `nix-shell` provides a dev environment with cabal-build, ghcid,
    hlint, brittany, and other tools.

-   you need to procure
    [`dhall-to-cabal`](https://github.com/dhall-lang/dhall-to-cabal)
    separately. It's necessary if you want to edit the cabal file, which
    is done through [`./cabal.dh`](./cabal.dh).

-   `direnv allow` for [lorri](https://github.com/target/lorri)
    integration.

-   use the `./pre-commit.sh` hook.
