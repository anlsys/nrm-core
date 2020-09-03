{ stdenv, src, autoreconfHook, fetchgit, zeromq, gfortran, pkgconfig, mpich2
, llvmPackages, czmq }:
stdenv.mkDerivation {
  inherit src;
  name = "libnrm";
  nativeBuildInputs = [ autoreconfHook pkgconfig mpich2 ];
  buildInputs = [ zeromq czmq gfortran mpich2 llvmPackages.openmp ];

  configureFlags =
    [ "--enable-pmpi" "CC=mpicc" "FC=mpifort" "CFLAGS=-fopenmp" ];
}
