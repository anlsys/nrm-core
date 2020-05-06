{ stdenv, fetchgit, mpich2, nix-update-source, libnrm, nrmSupport ? false }:

let inherit (stdenv.lib) optional;
in stdenv.mkDerivation {
  src = if nrmSupport then
    (nix-update-source.fetch ./nrm.json).src
  else
    (nix-update-source.fetch ./raw.json).src;
  name = "AMG";
  buildInputs = [ mpich2 ] ++ (optional nrmSupport libnrm);
  installPhase = ''
    mkdir -p $out/bin
    cp test/amg $out/bin
  '';
}
