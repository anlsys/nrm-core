# this file was tested using GNUMAKE >= 4.2.1.

# this is necessary for using multi-line strings as command arguments.
SHELL := $(shell which bash)

# this allows omitting newlines.
.ONESHELL:

.PHONY: ci
ci:
	@nix-shell -p yq -p jq --run bash <<< '
		for jobname in $$(yq -r "keys| .[]" .gitlab-ci.yml); do
			if [ "$$jobname" != "stages" ]; then
				gitlab-runner exec shell "$$jobname"
			fi
		done
	'

ci-%:
	@nix-shell -A hack --run bash <<< '
		gitlab-runner exec shell "$*"
	'

.PHONY: ghcid
ghcid: ghcid-nrmlib

ghcid-%: hsnrm/hsnrm.cabal hsnrm/.hlint.yaml
	@nix-shell -A hack --pure --run bash <<< '
		ghcid -C hsnrm --command "cabal v2-repl $* " --restart=holt.cabal  -l
	'

.PHONY: pre-commit
pre-commit: ormolu dhall-format shellcheck black codegen doc build pyclient notebooks

.PHONY: shellcheck
shellcheck:
	@nix-shell --pure -p '(import ./. {}).fd' '(import ./. {}).shellcheck' --run bash <<< '
		for F in $$(fd -E hsnrm/hbandit -E hsnrm/dhall-haskell -e sh); do
			shellcheck -s bash $$F
		done
	'

.PHONY: dhall-format
dhall-format:
	@nix-shell --pure -p '(import ./. {}).fd' '(import ./. {}).haskellPackages.dhall' --run bash <<< '
		for F in $$(fd -E hsnrm/hbandit -E hsnrm/dhall-haskell -e dhall); do
			dhall format --inplace $$F
		done
	'

.PHONY: ormolu
ormolu:
	@nix-shell --pure -E '
		let pkgs = (import ./. {});
		in pkgs.mkShell {
			buildInputs = [pkgs.fd pkgs.ormolu];
			shellHook =
				"export LOCALE_ARCHIVE=$${pkgs.glibcLocales}/lib/locale/locale-archive \n" +
				"export LANG=en_US.UTF-8";
		}
	' --run bash <<< '
		for F in $$(fd -E hsnrm/hbandit -E hsnrm/dhall-haskell -e hs); do
			ormolu -o -XTypeApplications -o -XPatternSynonyms -m inplace $$F
		done
	'

.PHONY: black
black:
	@nix-shell --pure -p '(import ./. {}).python37Packages.black' --run bash <<< '
		black pynrm/bin/*
		black pynrm/nrm/*.py
	'

.PHONY: codegen
codegen:
	@nix-shell --pure -A hack --run <<< bash '
		cd hsnrm
		cabal v2-run --builddir=../.build_codegen codegen ../resources/
	'

.PHONY: doc
doc:
	@nix-shell --pure -A hack --run <<< bash '
		cd hsnrm
		cabal v2-haddock nrm.so --haddock-internal --builddir=../.build
	'

.PHONY: vendor
vendor:
	@nix-shell --pure -A hack --run <<< bash '
		cp $$CABALFILE hsnrm/hsnrm.cabal
		cp $$CABALFILE_LIB dev/pkgs/hnrm/lib.cabal
		cp $$CABALFILE_BIN dev/pkgs/hnrm/bin.cabal
		cp $$NIXFILE_LIB/default.nix dev/pkgs/hnrm/lib.nix
		cp $$NIXFILE_BIN/default.nix dev/pkgs/hnrm/bin.nix
		chmod +rw hsnrm/hsnrm.cabal dev/pkgs/hnrm/bin.nix dev/pkgs/hnrm/lib.nix dev/pkgs/hnrm/bin.cabal dev/pkgs/hnrm/lib.cabal
		sed -i 's/src = .*/inherit src;/' dev/pkgs/hnrm/lib.nix
		sed -i 's/src = .*/inherit src;/' dev/pkgs/hnrm/bin.nix
	'


.PHONY: notebooks
notebooks:
	@nix-shell --pure -A hack --run <<< bash '
		notebooks/batchnb.py notebooks/configuration.ipynb
		jupyter nbconvert doc/notebooks/notebooks/configuration.ipynb --output-dir=doc/notebooks/notebooks
		rm doc/notebooks/notebooks/configuration.ipynb
		jupyter nbconvert notebooks/tutorial.ipynb --output-dir=doc/notebooks/notebooks
		jupyter nbconvert notebooks/internal-control.ipynb --output-dir=doc/notebooks/notebooks
	'

dhrun-%:
	rm -f hsnrm/.ghc*
	@nix-shell --pure -p '(import ./. {}).nrm' -p '(import ./. {}).dhrun' -p bash --run "dhrun -i" <<< '
		let all = ./dev/dhrun/all-tests.dh
			"../dev/dhrun/assets/"
			"../resources/defaults/Cfg.dhall // { verbose=<Normal|Verbose|Debug>.Debug }"
			"../resources/examples/"
		in all.exitcode
	'

.PHONY:client
client:
	@nix-shell --pure -A hack --run bash <<< '
		cd hsnrm
		cabal v2-build nrm --builddir=../.build
	'

.PHONY: build
build:
	@nix-shell --pure -A hack --run runhaskell <<< '
	{-# LANGUAGE NoImplicitPrelude #-}
	{-# LANGUAGE OverloadedStrings #-}

	import Control.Monad
	import Data.Text (dropEnd, lines, splitOn, strip)
	import Development.Shake hiding (getEnv)
	import Development.Shake.FilePath
	import Options.Applicative as OA
	import Protolude
	import System.Directory
	import System.Environment (getEnv, withArgs)
	import System.FilePath.Glob
	import qualified System.IO as SIO ( BufferMode (..), hSetBuffering, stdout,)
	import System.Posix.Process
	import System.Process.Typed
	import qualified Prelude

	main :: IO()
	main = do
	  version <- toS . strip . toS <$$> readProcessStdout_ "ghc --numeric-version"
	  ghcPathRaw <-  strip . toS <$$> readProcessStdout_ "which ghc"
	  let ghcPath = dropEnd 8 ghcPathRaw
	  runProcess_ $$ setWorkingDir "hsnrm" $$ shell "cp -f $$CABALFILE hsnrm.cabal"
	  runProcess_ . setWorkingDir "hsnrm" $$ proc "cabal" [ "v2-build", "nrm.so", "--ghc-option=-lHSrts_thr-ghc" <> version, "--ghc-option=-L" <> toS ghcPath <> "/lib/ghc-" <> version <> "/rts/", "--builddir=../.build", "--jobs=4" ]
	'

.PHONY: pyclient
pyclient:
	@nix-shell --pure -A hack --run runhaskell <<< '
	{-# LANGUAGE NoImplicitPrelude #-}
	{-# LANGUAGE OverloadedStrings #-}

	import Control.Monad
	import Data.Text (dropEnd, lines, splitOn, strip)
	import Development.Shake hiding (getEnv)
	import Development.Shake.FilePath
	import Options.Applicative as OA
	import Protolude
	import System.Directory
	import System.Environment (getEnv, withArgs)
	import System.FilePath.Glob
	import qualified System.IO as SIO ( BufferMode (..), hSetBuffering, stdout,)
	import System.Posix.Process
	import System.Process.Typed
	import qualified Prelude

	main :: IO()
	main = do
	  version <- toS . strip . toS <$$> readProcessStdout_ "ghc --numeric-version"
	  ghcPathRaw <-  strip . toS <$$> readProcessStdout_ "which ghc"
	  let ghcPath = dropEnd 8 ghcPathRaw
	  runProcess_ . setWorkingDir "hsnrm" $$ proc "cabal" [ "v2-build", "pynrm.so", "--ghc-option=-lHSrts_thr-ghc" <> version, "--ghc-option=-L" <> toS ghcPath <> "/lib/ghc-" <> version <> "/rts/", "--builddir=../.build", "--jobs=4" ]
	'

