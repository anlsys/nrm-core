{ stdenv, gfortran, class ? "E" }:

stdenv.mkDerivation {
  src = fetchTarball {
    url = "https://www.nas.nasa.gov/assets/npb/NPB3.4.1.tar.gz";
    sha256 = "1cc3vblkr4kwx9kf95rj0bp3ryapdpymqpbhfdd7jl8k4r6fcagi";
  };
  name = "NAS";
  buildInputs = [ gfortran ];
  installPhase = ''
    ls
    cd NPB3.4-OMP/config
    cp NAS.samples/make.def_gcc make.def
    cd ..
    make ep CLASS=A
    make ep CLASS=B
    make ep CLASS=C
    make ep CLASS=D
    make ep CLASS=E
    mkdir -p $out
    cp -r bin/ $out
  '';
}
