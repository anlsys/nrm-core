{ stdenv, src, autoreconfHook, fetchgit, zeromq, gfortran, pkgconfig, openmpi
, llvmPackages, hsnrm }:
stdenv.mkDerivation {
  inherit src;
  name = "libnrm";
  nativeBuildInputs = [ autoreconfHook pkgconfig ];
  buildInputs = [ zeromq gfortran openmpi llvmPackages.openmp ];
}
