{ stdenv, fetchgit, mpich2, cmake, liblapack, openblasCompat, fftw, libxml2
, hdf5, pythonPackages, hdf5-fortran, gfortran, boost, libnrm
, nrmSupport ? false }:
let inherit (stdenv.lib) optional optionals;
in stdenv.mkDerivation {
  src = fetchgit ({
    url = "https://xgitlab.cels.anl.gov/argo/applications/qmcpack.git";
  } // (if nrmSupport then {
    rev = "refs/heads/progress-nrm";
    sha256 = "0ij1xy0al02m5236srlfs078wlsiijp2kcmsgk4yvknavknbvn5n";
  } else {
    rev = "refs/heads/master";
    sha256 = "0fdlvmg0ri4mpwdxx4ky8mxc4rlkrq25smqld8b02lgjw1dvclki";
  }));

  name = "qmcpack";
  cmakeFlags = [
    "-DCMAKE_C_COMPILER=${mpich2}/bin/mpicc"
    "-DCMAKE_CXX_COMPILER=${mpich2}/bin/mpic++"
    "-DQMC_OMP=0"
    "-DCFLAGS=-O2"
  ] ++ optionals nrmSupport [
    "-DQMC_INCLUDE=${libnrm}/include/"
    "-DQMC_EXTRA_LIBS=${libnrm}/lib/libnrm.so"
  ];
  nativeBuildInputs = [ cmake mpich2 ];
  buildInputs = [
    liblapack
    openblasCompat
    fftw
    libxml2
    hdf5
    hdf5-fortran
    pythonPackages.numpy
    gfortran
    boost
  ] ++ (optional nrmSupport libnrm);
  propagatedBuildInputs = [ pythonPackages.numpy ];
  enableParallelBuilding = true;
  dontUseCmakeBuildDir = true;
  preConfigure = ''
    cmakeDir=$PWD
    cd build
  '';

  installPhase = ''
    mkdir -p $out
    cp -r * $out
  '';

  LIBXML2_HOME = "${libxml2}/lib";
}
