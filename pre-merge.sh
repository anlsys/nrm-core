#!/usr/bin/env bash
# shellcheck disable=SC2016

nix-shell --pure -E '
  let pkgs = (import ./.);
  in pkgs.mkShell {
    buildInputs = [pkgs.fd pkgs.ormolu];
    shellHook =
      "export LOCALE_ARCHIVE=${pkgs.glibcLocales}/lib/locale/locale-archive \n" +
      "export LANG=en_US.UTF-8";
  }
' --run bash <<< '
  for F in $(fd -E hsnrm/hbandit -E hsnrm/dhall-haskell -e hs); do
    ormolu -o -XTypeApplications -m inplace $F
  done
'

nix-shell --pure -p '(import ./.).fd' '(import ./.).haskellPackages.dhall' --run bash <<< '
  for F in $(fd -E hsnrm/hbandit -E hsnrm/dhall-haskell -e dhall); do
    dhall format --inplace $F
  done
'

nix-shell --pure -p '(import ./.).fd' '(import ./.).shellcheck' --run bash <<< '
  for F in $(fd -E hsnrm/hbandit -E hsnrm/dhall-haskell -e sh); do
    shellcheck -s bash $F
  done
'

nix-shell --pure -p '(import ./.).pythonPackages.black' --run bash <<< '
  black pynrm/bin/*
  black pynrm/nrm/*.py
'

./shake.sh doc
./shake.sh codegen
./shake.sh build
./shake.sh pyclient
./shake.sh notebooks
