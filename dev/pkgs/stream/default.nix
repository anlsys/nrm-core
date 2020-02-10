{ stdenv, gcc, src, fetchgit, gfortran, pkgconfig, libnrm, nix-update-source
, problemSize ? "80000000", iterationCount ? "10", nrmSupport ? false }:

let inherit (stdenv.lib) optional;

in stdenv.mkDerivation {
  src = if nrmSupport then
    (nix-update-source.fetch ./nrm.json).src
  else
    (nix-update-source.fetch ./raw.json).src;
  name = "stream";

  nativeBuildInputs = [ gfortran pkgconfig ];
  buildInputs = optional nrmSupport libnrm;

  preBuild =
    "substituteInPlace Makefile --replace 'CFLAGS =' 'CFLAGS = -O2 -march=native -DSTREAM_ARRAY_SIZE=${problemSize} -DNTIMES=${iterationCount}'";

  buildFlags = "CC=${gcc}/bin/gcc FC=${gfortran}/bin/gfortran";

  installPhase = ''
    mkdir -p $out/bin/
    cp stream_c.exe $out/bin/stream_c
    cp stream_f.exe $out/bin/stream_f
  '';
}
