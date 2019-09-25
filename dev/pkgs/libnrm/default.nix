{ stdenv, src, autoreconfHook, fetchgit, zeromq, gfortran, pkgconfig, mpich2}:
stdenv.mkDerivation {
  inherit src;
  name = "libnrm";
  nativeBuildInputs = [ autoreconfHook pkgconfig ];
  buildInputs = [ zeromq gfortran mpich2 ] ;
}
