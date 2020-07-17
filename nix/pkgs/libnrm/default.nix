{ stdenv, src, autoreconfHook, fetchgit, zeromq, gfortran, pkgconfig, openmpi
, llvmPackages, czmq }:
stdenv.mkDerivation {
  inherit src;
  name = "libnrm";
  nativeBuildInputs = [ autoreconfHook pkgconfig openmpi ];
  buildInputs = [ zeromq czmq gfortran openmpi llvmPackages.openmp ];

  configureFlags =
    [ "--enable-pmpi" "CC=mpicc" "FC=mpifort" "CFLAGS=-fopenmp" ];
}
