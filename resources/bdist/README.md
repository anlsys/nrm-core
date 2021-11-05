eow to prepare binaries for nrm-core
m===================================

Because Spack doesn't support Haskell packages, and it would probably be too
much work anyway, we currently prefer binary releases for this repository.

To ensure that the binaries have a reasonable amount of dependencies on
external shared libraries, we unfortunately need to work a little: ghc needs to
be patched and recompiled.

This directory contains everything needed. The instructions are:

	# use ghcup to install the right compiler
	ghcup install ghc 8.6.5
	ghcup compile ghc -j 4 -v 8.6.5 -b 8.6.5 --set -c `pwd`/resources/bdist/build.mk -p `pwd`/resources/bdist

	# install local libraries
	sudo apt-get install pkg-config libzmq3-dev

	# build
	cabal v2-update
	cabal v2-build all

	# package into a decent archive
	version=`git describe --always`
	mkdir nrm-core-${version}
	cd nrm-core-${version}	
	mkdir -p bin
	mkdir -p lib
	cp dist-newstyle/build/x86_64-linux/ghc-8.6.5/hsnrm-bin-*/x/nrm/build/nrm/nrm ./bin/
	cp dist-newstyle/build/x86_64-linux/ghc-8.6.5/hsnrm-bin-*/f/nrm-core/build/nrm-core/libnrm-core.so ./lib/
	cp dist-newstyle/build/x86_64-linux/ghc-8.6.5/hsnrm-extra-*/f/nrm-core-python/build/nrm-core-python/libnrm-core-python.so ./lib/
	chmod 644 ./lib/*
	chmod 755 ./bin/*
	tar -czvf nrm-core-${version}.tar.gz nrm-core-${version}

The resulting archive can be installed easily with Spack.
