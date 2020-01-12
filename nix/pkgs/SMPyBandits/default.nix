{ fetchFromGitHub, pythonPackages }:
pythonPackages.buildPythonPackage {
  src = fetchFromGitHub {
    owner = "SMPyBandits";
    repo = "SMPyBandits";
    rev = "deb2e347e3e42f36e70938c7a73a245a98615356";
    sha256 = "1l9xlglr1f82hmsfnpwmq8jf02yrzndciiz9xv8jbkafki266ll3";
  };
  name = "SMPYBandits";
  buildInputs = [ ];
  propagatedBuildInputs = with pythonPackages; [
    numpy
    numba
    scipy
    matplotlib
    seaborn
    tqdm
    scikitlearn
    scikit-optimize
  ];
  checkPhase = "true";
}
