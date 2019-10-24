{ stdenv, src, fetchgit, gfortran, pkgconfig, libnrm, nix-update-source
, problemSize ? "80000000", iterationCount ? "20", nrmSupport ? false }:

let inherit (stdenv.lib) optional;

in stdenv.mkDerivation {
  src = (nix-update-source.fetch ./pin.json).src;
  name = "stream";

  nativeBuildInputs = [ gfortran pkgconfig ];
  buildInputs = optional nrmSupport libnrm;

  preBuild =
    "substituteInPlace Makefile --replace 'CFLAGS =' 'CFLAGS = -O2 -march=native -DSTREAM_ARRAY_SIZE=${problemSize} -DNTIMES=${iterationCount}'";

  buildFlags = "CC=gcc FC=gfortran";

  installPhase = ''
    mkdir -p $out/bin/
    cp stream_c.exe $out/bin/stream_c
    cp stream_f.exe $out/bin/stream_f
  '';
}
