{ stdenv, src, autoreconfHook, fetchgit, zeromq, gfortran, pkgconfig, openmpi
, llvmPackages, hsnrm }:
stdenv.mkDerivation {
  inherit src;
  name = "libnrm";
  nativeBuildInputs = [ autoreconfHook pkgconfig ];
  buildInputs = [ zeromq gfortran openmpi llvmPackages.openmp ];
  CC = "mpicc";
  FC = "mpifort";
  CFLAGS = "-fopenmp";
  configureFlags = [ "--enable-pmpi" ];
}
